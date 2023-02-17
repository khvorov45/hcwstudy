library(tidyverse)

vac_hist <- read_csv("data/vaccinations.csv", col_types = cols())

prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        .groups = "drop",
        prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
        prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
        prior2023 = sum(year >= 2018 & year < 2023 & (status == "Australia" | status == "Overseas")),
    )

serology <- read_csv("data/serology.csv", col_types = cols()) %>%
    filter(vax_inf == "V") %>%
    left_join(prior_vac_counts, "pid") %>%
    mutate(
        prior_study_year = case_when(
            year == 2020 ~ prior2020,
            year == 2021 ~ prior2021,
            year == 2022 ~ prior2022,
            year == 2023 ~ prior2023,
            TRUE ~ NA_integer_
        ),
        subtype = factor(subtype, c("H1", "H3", "BVic", "BYam"))
    )

summarise_prop <- function(success, total) {
  ci <- PropCIs::exactci(success, total, 0.95)
  prop <- success / total
  low <- ci$conf.int[[1]]
  high <- ci$conf.int[[2]]
  f <- function(x) round(x * 100)
  tibble(
    prop, low, high,
    comb = glue::glue("{f(prop)}% ({f(low)}%, {f(high)}%)")
  )
}

seropos_summary <- serology %>%
    group_by(year, day, virus, subtype, virus_egg_cell, virus_clade) %>%
    summarise(summarise_prop(sum(titre >= 40), n()), .groups = "drop") %>%
    mutate(prior_study_year = "Combined") %>%
    bind_rows(
        serology %>%
            group_by(year, day, virus, subtype, virus_egg_cell, virus_clade, prior_study_year) %>%
            summarise(summarise_prop(sum(titre >= 40), n()), .groups = "drop") %>%
            mutate(prior_study_year = as.character(prior_study_year))
    ) %>%
    filter(
        day != 7,
        !(year == 2020 & virus == "A/Victoria/2570/2019e"),
        !(year == 2021 & virus == "A/Brisbane/02/2018e")
    )

seropos_plot <- seropos_summary %>%
    group_by(subtype) %>%
    group_map(.keep = TRUE, function(data, key) {
        pl <- data %>%
            ggplot(aes(prior_study_year, prop)) +
            theme_bw() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "top",
                panel.spacing = unit(0, "null"),
                strip.background = element_blank(),
            ) +
            facet_grid(subtype + virus_egg_cell ~ year) +
            coord_cartesian(ylim = c(0, 1)) +
            scale_color_viridis_d("Day", begin = 0.2, end = 0.8, option = "A") +
            scale_y_continuous("Seropositive (95% CI)", labels = scales::percent_format(), breaks = c(0.2, 0.5, 0.8)) +
            scale_x_discrete("Known prior vaccinations in the 5 years before bleed") +
            geom_pointrange(aes(ymin = low, ymax = high, col = factor(day)), position = position_dodge(width = 0.5), size = 0.1) +
            geom_text(
                aes(x = 0, y = 0, label = lab), 
                data %>% mutate(lab = paste(virus, virus_clade)) %>% select(year, virus_egg_cell, subtype, lab) %>% distinct(),
                hjust = 0, vjust = 0, size = 2,
            )
        attr(pl, "key") <- key
        pl
    })

walk(seropos_plot, function(pl) {
    sb <- attr(pl, "key")$subtype
    nm <- paste0("report/seropos/seropos-", sb)
    (function(name, ...) {ggsave(paste0(name, ".pdf"), ...);ggsave(paste0(name, ".png"), ...)})(
        nm, pl, width = 15, height = 10, units = "cm"
    )
})

serology_ratios <- serology %>%
    pivot_wider(names_from = "day", values_from = "titre") %>%
    mutate(ratio = `14` / `0`) %>%
    filter(!is.na(ratio))

seroconv_summary <- serology_ratios %>%
    group_by(year, virus, subtype, virus_egg_cell, virus_clade) %>%
    summarise(summarise_prop(sum(ratio >= 4), n()), .groups = "drop") %>%
    mutate(prior_study_year = "Combined") %>%
    bind_rows(
        serology_ratios %>%
            group_by(year, virus, subtype, virus_egg_cell, virus_clade, prior_study_year) %>%
            summarise(summarise_prop(sum(ratio >= 4), n()), .groups = "drop") %>%
            mutate(prior_study_year = as.character(prior_study_year))
    ) %>%
    filter(
        !(year == 2020 & virus == "A/Victoria/2570/2019e"),
        !(year == 2021 & virus == "A/Brisbane/02/2018e")
    )

seroconv_plot <- seroconv_summary %>%
    group_by(subtype) %>%
    group_map(.keep = TRUE, function(data, key) {
        pl <- data %>%
            ggplot(aes(prior_study_year, prop)) +
            theme_bw() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "top",
                panel.spacing = unit(0, "null"),
                strip.background = element_blank(),
            ) +
            facet_grid(subtype + virus_egg_cell ~ year) +
            coord_cartesian(ylim = c(0, 1)) +
            scale_y_continuous("Seroconverted (day 14 / day 0 >= 4) (95% CI)", labels = scales::percent_format(), breaks = c(0.2, 0.5, 0.8)) +
            scale_x_discrete("Known prior vaccinations in the 5 years before bleed") +
            geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 0.5), size = 0.1) +
            geom_text(
                aes(x = 0, y = 0, label = lab), 
                data %>% mutate(lab = paste(virus, virus_clade)) %>% select(year, virus_egg_cell, subtype, lab) %>% distinct(),
                hjust = 0, vjust = 0, size = 2,
            )
        attr(pl, "key") <- key
        pl
    })

walk(seroconv_plot, function(pl) {
    sb <- attr(pl, "key")$subtype
    nm <- paste0("report/seropos/seroconv-", sb)
    (function(name, ...) {ggsave(paste0(name, ".pdf"), ...);ggsave(paste0(name, ".png"), ...)})(
        nm, pl, width = 15, height = 10, units = "cm"
    )
})
