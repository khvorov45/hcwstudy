library(tidyverse)

vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())
participants <- read_csv("data/participants.csv", col_types = cols())
serology <- read_csv("data/serology.csv", col_types = cols())

priors <- vaccinations %>%
    summarise(
        .by = pid,
        prior2020 = sum((status == "Australia" | status == "Overseas") & (year < 2020 & year >= 2020 - 5)),
        prior2021 = sum((status == "Australia" | status == "Overseas") & (year < 2021 & year >= 2021 - 5)),
        prior2022 = sum((status == "Australia" | status == "Overseas") & (year < 2022 & year >= 2022 - 5)),
    ) %>%
    pivot_longer(-pid, names_to = "year", values_to = "priors") %>%
    mutate(year = str_replace(year, "prior", "") %>% as.integer())

serology_relevant <- serology %>%
    filter(vax_inf == "V", subtype == "H1" | subtype == "H3", virus_vaccine, day %in% c(0, 14, 220)) %>%
    select(-vax_inf, -virus_vaccine) %>%
    left_join(participants %>% select(pid, dob, age_screening), "pid") %>%
    mutate(
        age_group = cut(age_screening, c(-Inf, 30, 50, Inf)),
        birth_cohort = cut(dob, c(ymd("1800-01-01"), ymd("1967-01-01"), ymd("2100-01-01")), right = FALSE),
    ) %>%
    left_join(priors, c("pid", "year"))

# NOTE(sen) Should be empty
filter(serology_relevant, .by = c(pid, year, day, subtype, virus_egg_cell), n() > 1)

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

gen_gmt_plot <- function(data) {
    data %>%
        mutate(xcoord = as.integer(factor(day)) + (year - 2020) * 3) %>%
        ggplot(aes(xcoord, mean, col = subtype, shape = subtype, lty = subtype)) + 
        theme_bw() +
        theme(
            legend.position = "bottom",
            panel.spacing = unit(0, "null"),
            strip.background = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1),
            panel.grid.minor = element_blank(),
        ) +
        scale_y_log10("GMT", breaks = 5 * 2^(0:15)) +
        scale_x_continuous(
            "Year/day", breaks = 1:9, 
            labels = expand.grid(day = c(0, 14, 220), year = 2020:2022) %>% mutate(str = paste0(year, " d", day)) %>% pull(str),
        ) +
        scale_color_manual("Subtype", values = c("#00964b", "#9e21d5")) +
        scale_shape_discrete("Subtype") +
        scale_linetype_discrete("Subtype") +
        geom_line() +
        geom_pointrange(aes(ymin = low, ymax = high))
}

gmt_plot <- serology_relevant %>%
    summarise(.by = c(year, day, subtype, virus_egg_cell), summarise_logmean(titre)) %>%
    gen_gmt_plot() +
    facet_wrap(~virus_egg_cell, strip.position = "right", ncol = 1)

ggsave("h1-h3-diff/gmt-combined.pdf", gmt_plot, width = 10, height = 10, units = "cm")

gmt_plot_agegroup <- serology_relevant %>%
    filter(!is.na(age_group)) %>%
    summarise(.by = c(year, day, subtype, virus_egg_cell, age_group), summarise_logmean(titre)) %>%
    gen_gmt_plot() +
    facet_grid(virus_egg_cell ~ age_group)

ggsave("h1-h3-diff/gmt-combined-agegroup.pdf", gmt_plot_agegroup, width = 20, height = 10, units = "cm")

gmt_plot_birth_cohort <- serology_relevant %>%
    filter(!is.na(birth_cohort)) %>%
    summarise(.by = c(year, day, subtype, virus_egg_cell, birth_cohort), summarise_logmean(titre)) %>%
    mutate(birth_cohort = paste("after", birth_cohort)) %>%
    gen_gmt_plot() +
    facet_grid(virus_egg_cell ~ birth_cohort)

ggsave("h1-h3-diff/gmt-combined-birthcohort.pdf", gmt_plot_birth_cohort, width = 20, height = 10, units = "cm")

gmt_plot_priors <- serology_relevant %>%
    summarise(.by = c(year, day, subtype, virus_egg_cell, priors), summarise_logmean(titre)) %>%
    gen_gmt_plot() +
    facet_grid(virus_egg_cell ~ priors)

ggsave("h1-h3-diff/gmt-combined-priors.pdf", gmt_plot_priors, width = 30, height = 10, units = "cm")
