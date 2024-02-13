library(tidyverse)

vac_hist <- read_csv("./data/vaccinations.csv", col_types = cols())

prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        .groups = "drop",
        prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
        prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
        prior2023 = sum(year >= 2018 & year < 2023 & (status == "Australia" | status == "Overseas")),
    )

participants <- read_csv("data/participants.csv", col_types = cols())

serology_allh1egg <- read_csv("data/serology.csv", col_types = cols()) %>%
    filter(subtype == "H1", virus_egg_cell == "egg", vax_inf == "V") %>%
    left_join(participants, "pid") %>%
    left_join(prior_vac_counts, "pid") %>%
    mutate(
        prior_study_year = case_when(
            year == 2020 ~ prior2020,
            year == 2021 ~ prior2021,
            year == 2022 ~ prior2022,
            year == 2023 ~ prior2023,
            TRUE ~ NA_integer_
        ),
        age_cat = cut(age_screening, c(-Inf, 30, 40, 50, Inf))
    )

have_newcal_and_brazil <- serology_allh1egg %>%
    filter(virus %in% c("A/NewCaledonia/20/1999e", "A/Brazil/11/1978e")) %>%
    select(pid) %>%
    distinct()

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

gmt_gmr_filter <- function(data) {
    data %>% filter(
        pid %in% have_newcal_and_brazil$pid | 
        (year %in% c(2021, 2022) & prior_study_year == 0) |
        (year %in% c(2022) & prior_study_year == 1),
        (year == 2020 & virus == "A/Brisbane/02/2018e") |
        (year == 2021 & virus == "A/Victoria/2570/2019e") |
        (year == 2022 & virus == "A/Victoria/2570/2019e")
    )
}

gmt_gmr_pointrange <- function(data) {
    data %>%
        mutate(year = factor(year)) %>%
        ggplot(aes(prior_study_year, mean, col = year, shape = year)) +
        theme_bw() +
        theme(
            legend.position = "top",
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
        ) +
        scale_x_continuous("Prior vaccinations in study year") +
        scale_color_viridis_d(begin = 0.2, end = 0.8, option = "A") +
        geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 0.3))
}

serology_allh1egg_gmts <- serology_allh1egg %>%
    filter(day %in% c(0, 14, 220)) %>%
    gmt_gmr_filter() %>%
    group_by(prior_study_year, year, day) %>%
    summarise(summarise_logmean(titre), .groups = "drop") %>%
    gmt_gmr_pointrange() +
    facet_wrap(~day, ncol = 1, strip.position = "right") +
    scale_y_log10("GMT (95% CI)", breaks = 5 * 2^(0:15))

ggsave("steph-h1/gmts.pdf", serology_allh1egg_gmts, width = 10, height = 15, units = "cm")

serology_allh1egg_gmrs <- serology_allh1egg %>%
    filter(day %in% c(0, 14)) %>%
    gmt_gmr_filter() %>%
    pivot_wider(names_from = "day", values_from = "titre") %>%
    mutate(ratio = `14` / `0`) %>%
    group_by(prior_study_year, year) %>%
    summarise(summarise_logmean(ratio), .groups = "drop") %>%
    gmt_gmr_pointrange() +
    scale_y_log10("GMR (95% CI)", breaks = c(1, 2, 4 * 2^(0:15)))

ggsave("steph-h1/gmrs.pdf", serology_allh1egg_gmrs, width = 10, height = 10, units = "cm")

serology_allh1egg_extra <- serology_allh1egg %>%
    filter(
        pid %in% have_newcal_and_brazil$pid | (year == 2021 & prior_study_year == 0),
        (year == 2020 & virus == "A/Brisbane/02/2018e") |
        (year == 2021 & virus == "A/Victoria/2570/2019e") |
        (virus %in% c("A/NewCaledonia/20/1999e", "A/Brazil/11/1978e")),
        day %in% c(0, 14)
    ) %>%
    pivot_wider(names_from = "day", values_from = "titre") %>%
    mutate(ratio = `14` / `0`) %>%    
    group_by(virus, year, prior_study_year, age_cat) %>%
    summarise(summarise_logmean(ratio), .groups = "drop") %>%
    mutate(
        virus_lab = recode(
            virus,
            "A/Brisbane/02/2018e" = "Vax",
            "A/Victoria/2570/2019e" = "Vax",
            "A/NewCaledonia/20/1999e" = "NewCal99",
            "A/Brazil/11/1978e" = "Br78",
        ) %>% 
            factor(c("Vax", "NewCal99", "Br78")),
        year = factor(year), prior_study_year = factor(prior_study_year)
    ) %>%
    ggplot(aes(virus_lab, mean, col = prior_study_year, shape = prior_study_year)) +
    theme_bw() +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
    ) +
    facet_grid(age_cat ~ year) +
    scale_x_discrete("Virus") +
    scale_color_viridis_d(begin = 0.2, end = 0.8, option = "A") +
    scale_y_log10("GMR (95% CI)", breaks = c(1, 2, 4 * 2^(0:15))) +
    geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 0.3))

ggsave("steph-h1/extra.pdf", serology_allh1egg_extra, width = 20, height = 20, units = "cm")
