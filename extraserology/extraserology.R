library(tidyverse)

vac_hist <- read_csv("./data/vaccinations.csv", col_types = cols())

prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
        prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
    )

# TODO(sen) Probably have prior_vac_counts as a separate table
titres <- read_csv("data/serology.csv", col_types = cols()) %>%
    filter(subtype == "H1", year %in% 2020:2021, virus_egg_cell == "egg") %>%
    left_join(read_csv("data/participants.csv", col_types = cols()), "pid") %>%
    left_join(prior_vac_counts, "pid") %>%
    mutate(
        prior_vax_in_serum_year = if_else(year == 2020, prior2020, prior2021),
        titre_type = if_else(
            (year == 2020 & virus == "A/Brisbane/02/2018e") | (year == 2021 & virus == "A/Victoria/2570/2019e"),
            "vaccine", "other" 
        ) %>% 
            factor(c("vaccine", "other"))
    )

titres %>% count(year, titre_type, virus)

titres_plot <- titres %>% 
    mutate(day = if_else(day == 220, 50, day)) %>%
    ggplot(aes(day, titre)) +
    theme_bw() +
    theme(
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "null"),
        strip.background = element_blank(),
    ) +
    scale_y_log10("Titre", breaks = 5 * 2^(0:15)) +
    scale_x_continuous("Day", breaks = c(0, 7, 14, 50), labels = c(0, 7, 14, 220)) +
    facet_grid(titre_type ~ year) +
    geom_line(aes(group = pid), alpha = 0.05, color = "gray50") +
    geom_point(alpha = 0.05, color = "gray50", shape = 18)  + 
    geom_boxplot(aes(group = paste0(day, year, virus)), fill = NA, color = "blue", outlier.alpha = 0)

ggsave("extraserology/titres_plot.pdf", titres_plot, width = 12, height = 12, units = "cm")

ratios <- titres %>%
    pivot_wider(names_from = "day", values_from = "titre") %>%
    mutate(ratio = `14` / `0`)

ratios_fit <- lm(I(log(ratio)) ~ as.factor(year) * titre_type, ratios)
ratios_coefs <- coef(ratios_fit)
ratios_vcov <- vcov(ratios_fit)

calc_ratios <- function(names, year, type) {
    tibble(
        logrise = sum(ratios_coefs[names]), 
        variance = sum(ratios_vcov[names, names]),
        year = year, type = type,
    )
}

fit_result <- tibble(calc_ratios("(Intercept)", 2020, "vaccine")) %>%
    bind_rows(calc_ratios(c("(Intercept)", "titre_typeother"), 2020, "other")) %>%
    bind_rows(calc_ratios(c("(Intercept)", "as.factor(year)2021"), 2021, "vaccine")) %>%
    bind_rows(calc_ratios(c("(Intercept)", "titre_typeother", "as.factor(year)2021", "as.factor(year)2021:titre_typeother"), 2021, "other")) %>%
    mutate(
        se = sqrt(variance),
        logriselow = logrise - 1.96 * se,
        logrisehigh = logrise + 1.96 * se,
        rise = exp(logrise),
        riselow = exp(logriselow),
        risehigh = exp(logrisehigh),
        type = factor(type, c("vaccine", "other")),
    )

write_csv(fit_result, "extraserology/vaccine_response_fit.csv")

fit_plot <- fit_result %>%
    ggplot(aes(type, rise, ymin = riselow, ymax = risehigh)) +
    theme_bw() +
    theme(
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(0, "null"),
        axis.title.x = element_blank(),
    ) +
    scale_y_continuous("Rise", breaks = 0:15) +
    facet_wrap(~year) +
    geom_pointrange()

ggsave("extraserology/vaccine_response_plot.pdf", fit_plot, width = 7, height = 10, units = "cm")
