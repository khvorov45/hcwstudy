library(tidyverse)

sercovid <- read_csv("data/serology-covid.csv", col_types = cols())
covid_bleed_dates <- read_csv("data/covid-bleed-dates.csv", col_types = cols())
covid_vax <- read_csv("data/covid-vax.csv", col_types = cols())
flu_bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())
swabs <- read_csv("data/swabs.csv", col_types = cols())

covid_bleed_dates_to_join <- covid_bleed_dates %>%
    group_by(pid, day) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    select(pid, bleed_day_id = day, bleed_date = date) %>%
    mutate(bleed_flu_covid = "C", bleed_year = 2021)

flu_bleed_dates_to_join_2021 <- flu_bleed_dates %>%
    filter(year == 2021) %>%
    select(pid, bleed_year = year, bleed_date = date, bleed_day_id = day) %>%
    mutate(bleed_flu_covid = "F")

flu_bleed_dates_to_join_2020 <- flu_bleed_dates %>%
    filter(year == 2020, day == 220) %>%
    select(pid, bleed_year = year, bleed_date = date) %>%
    mutate(bleed_flu_covid = "F", bleed_day_id = 0)

sercovid_with_dates <- sercovid %>%
    filter(bleed_day_id %in% c(0, 14, 220), vax_inf == "V") %>%
    left_join(
        bind_rows(covid_bleed_dates_to_join, flu_bleed_dates_to_join_2021, flu_bleed_dates_to_join_2020),
        c("pid", "bleed_year", "bleed_day_id", "bleed_flu_covid")
    ) %>%
    group_by(pid, bleed_day_id) %>%
    filter(bleed_date == max(bleed_date)) %>%
    ungroup()

sercovid_with_dose2info <- sercovid_with_dates %>%
    left_join(covid_vax %>% filter(dose == 2) %>% select(pid, dose2_date = date, dose2_brand = brand), "pid")

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

day_brand_means <- sercovid_with_dose2info %>%
    group_by(bleed_day_id, dose2_brand) %>%
    summarise(summarise_logmean(ic50), .groups = "drop")

create_titre_plot <- function(data, day_breaks) {
    relevant_means <- day_brand_means %>%
        filter(bleed_day_id %in% day_breaks) %>%
        mutate(norm_bleed_day_id = if_else(bleed_day_id == day_breaks[[1]], 0, 1))

    data %>%
        filter(bleed_day_id %in% day_breaks) %>%
        group_by(bleed_day_id) %>%
        mutate(
            bleed_date_deviation = as.integer(bleed_date - mean(bleed_date, na.rm = TRUE)),
        ) %>%
        ungroup() %>%
        mutate(
            norm_bleed_day_id = if_else(bleed_day_id == day_breaks[[1]], 0, 1),
            norm_bleed_date_deviation = bleed_date_deviation / max(abs(bleed_date_deviation), na.rm = TRUE),
            xcoord = norm_bleed_day_id + norm_bleed_date_deviation * 0.2, #runif(n(), -0.1, 0.1),
            ycoord = exp(log(ic50) + runif(n(), -0.1, 0.1)),
        ) %>%
        ggplot(aes(xcoord, ycoord)) +
        theme_bw() +
        theme(
            strip.background = element_blank(),
            panel.grid.minor = element_blank(),
        ) +
        facet_wrap(~dose2_brand) +
        scale_y_log10("IC50") +
        scale_x_continuous("Day", breaks = c(0, 1), labels = day_breaks) +
        geom_line(aes(group = pid), alpha = 0.2) +
        geom_point(shape = 18, alpha = 0.3) +
        geom_boxplot(aes(x = norm_bleed_day_id, group = paste0(norm_bleed_day_id, dose2_brand)), fill = NA, outlier.alpha = 0, color = "blue", width = 0.3) +
        geom_line(
            aes(norm_bleed_day_id, mean),
            relevant_means,
            color = "red", linewidth = 1, lty = "31"
        ) +
        geom_pointrange(
            aes(norm_bleed_day_id, mean, ymin = low, ymax = high),
            relevant_means,
            color = "red"
        )
}

covid_serology_plot_day0_14 <- sercovid_with_dose2info %>%
    create_titre_plot(c(0, 14))

ggsave("covid-serology/covid-serology-day0_14-brand.pdf", covid_serology_plot_day0_14, units = "cm", width = 15, height = 10)

earliest_covid_swabs <- swabs %>%
    filter(swab_virus == "SARS-CoV-2", swab_result == 1) %>%
    group_by(pid) %>%
    summarise(earliest_covid_swab_date = min(samp_date))

covid_rises <- sercovid_with_dose2info %>%
    select(pid, bleed_day_id, ic50) %>%
    pivot_wider(names_from = "bleed_day_id", values_from = "ic50") %>%
    mutate(rise_day14_220 = `220` / `14`)# %>%
    # select(pid, contains("rise"))

covid_intervals <- sercovid_with_dose2info %>%
    select(pid, bleed_day_id, bleed_date) %>%
    pivot_wider(names_from = "bleed_day_id", values_from = "bleed_date") %>%
    mutate(interval_day14_220 = as.integer(`220` - `14`)) %>%
    select(pid, contains("interval"))

covid_serology_day14_220 <- sercovid_with_dose2info %>%
    left_join(earliest_covid_swabs, "pid") %>%
    filter(is.na(earliest_covid_swab_date) | earliest_covid_swab_date > bleed_date) %>%
    left_join(covid_vax %>% filter(dose == 3) %>% select(pid, dose3_date = date), "pid") %>%
    filter(bleed_day_id == 14 | dose3_date > bleed_date) %>%
    left_join(covid_rises, "pid") %>%
    filter(is.na(rise_day14_220) | rise_day14_220 < 9)

covid_serology_plot_day14_220 <- covid_serology_day14_220 %>% create_titre_plot(c(14, 220))

ggsave("covid-serology/covid-serology-day14_220-brand.pdf", covid_serology_plot_day14_220, units = "cm", width = 15, height = 10)

covid_serology_rises_day14_220_for_fit <- covid_serology_day14_220 %>%
    left_join(covid_intervals, "pid") %>%
    select(pid, dose2_brand, ic50, bleed_day_id, interval_day14_220, rise_day14_220) %>%
    pivot_wider(names_from = "bleed_day_id", values_from = "ic50") %>%
    mutate(postseason = log(`220`), postvax = log(`14`)) %>%
    select(pid, dose2_brand, interval_day14_220, postvax, postseason, rise_day14_220) %>%
    filter(complete.cases(.))

lm(postseason ~ postvax + interval_day14_220 + dose2_brand, covid_serology_rises_day14_220_for_fit) %>%
    broom::tidy()
