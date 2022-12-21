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

# TODO(sen) Exclude bleeds that don't make sense (e.g., baseline after vaccination)?
serology <- read_csv("data/serology.csv", col_types = cols()) %>%
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

summarise_logmean <- function(vec, round_to = 0) {
	vec <- na.omit(vec)
	total <- length(vec)
	log_vec <- log(vec)
	logmean <- mean(log_vec)
	logse <- sd(log_vec) / sqrt(total)
	logmargin <- 1.96 * logse
	loglow <- logmean - logmargin
	loghigh <- logmean + logmargin
	mean <- exp(logmean)
	low <- exp(loglow)
	high <- exp(loghigh)
	tibble(mean, low, high)
}

serology %>%
    filter(year == 2020, pid == "QCH-063", day %in% c(0, 14)) %>%
    select(day, virus, titre) %>%
    pivot_wider(names_from = "day", values_from = "titre")

serology_averages <- serology %>%
    group_by(day, year, subtype, virus, virus_egg_cell, prior_study_year) %>%
    summarise(
        .groups = "drop",
        summarise_logmean(titre)
    )

serology_averages_ylims <- c(min(serology_averages$mean), max(serology_averages$mean))

plots_averages <- serology_averages %>%
    filter(
        day != 7,
        !(subtype == "H1" & year == 2020 & str_starts(virus, "A/Victoria/2570/2019")),
        !(subtype == "H1" & year == 2021 & str_starts(virus, "A/Brisbane/02/2018")),
    ) %>%
    mutate(
        day = if_else(day == 220, 50, day),
        xpos = day + (prior_study_year - 2.5),
        prior_vac_factor = factor(prior_study_year),
        virus_egg_cell = tools::toTitleCase(virus_egg_cell),
    ) %>%
    group_by(subtype) %>%
    group_map(.keep = TRUE, function(onesubtype, key) {
        plot <- onesubtype %>%
            ggplot(aes(xpos, mean, ymin = low, ymax = high, color = prior_vac_factor, shape = prior_vac_factor)) +
            theme_bw() +
            theme(
                strip.background = element_blank(),
                panel.spacing = unit(0, "null"),
                panel.grid.minor = element_blank(),
                legend.position = "bottom"
            ) +
            facet_grid(year ~ virus_egg_cell) +
            scale_y_log10("Titre", breaks = 5 * 2^(0:20), expand = expansion(0.1, 0)) +
            scale_x_continuous("Day", breaks = c(0, 14, 50), labels = c(0, 14, 220), expand = expansion(0.1, 0)) +
            scale_color_manual("Prior vaccinations in study year", values = viridis::viridis_pal(option = "A", end = 0.8)(6)) +
            scale_shape_manual("Prior vaccinations in study year", values = c(8, 19, 17, 15, 18, 4)) +
            coord_cartesian(ylim = serology_averages_ylims) +
            geom_line() +
            geom_point() + 
            geom_text(
                aes(0, 10, label = virus), 
                data = onesubtype %>% select(year, virus_egg_cell, virus) %>% distinct(),
                inherit.aes = FALSE,
                hjust = 0, vjust = 1,
            )
        attr(plot, "subtype") <- key$subtype
        plot
    })

walk(plots_averages, function(plot) {
    subtype <- attr(plot, "subtype")
    ggsave(glue::glue("report/figure-titre/averages-{subtype}.pdf"), plot, width = 15, height = 15, units = "cm")
})
