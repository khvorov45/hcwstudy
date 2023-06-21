library(tidyverse)

serology <- read_csv("data/serology.csv", col_types = cols())
vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())

priors <- vaccinations %>%
    summarise(
        .by = c(pid), 
        prior2020 = sum((year >= 2020 - 5 & year < 2020) & (status == "Australia" | status == "Overseas")),
        prior2021 = sum((year >= 2021 - 5 & year < 2021) & (status == "Australia" | status == "Overseas")),
        prior2022 = sum((year >= 2022 - 5 & year < 2022) & (status == "Australia" | status == "Overseas")),
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

serology_h3_egg <- serology %>%
    filter(vax_inf == "V", subtype == "H3", virus_egg_cell == "egg", virus_vaccine) %>%
    filter(.by = c(pid, year), 14 %in% day, day %in% c(0, 14, 220)) %>%
    select(pid, year, day, titre)

# NOTE(sen) Should be empty
serology_h3_egg %>% filter(.by = c(pid, year, day), n() > 1)

earliest_priors <- priors %>%
    pivot_longer(-pid, names_to = "prior_year", values_to = "prior_value") %>%
    mutate(prior_year = str_replace(prior_year, "prior", "") %>% as.integer()) %>%
    left_join(
        serology_h3_egg %>% filter(day == 14) %>% select(pid, year) %>% mutate(have_d14_ser = TRUE),
        c("pid", "prior_year" = "year")
    ) %>%
    mutate(have_d14_ser = replace_na(have_d14_ser, FALSE)) %>%
    filter(have_d14_ser) %>%
    select(-have_d14_ser) %>%
    summarise(.by = c(pid, prior_value), earliest_prior_year = min(prior_year))

earliest_priors_05 <- earliest_priors %>%
    filter(prior_value == 0 | prior_value == 5)

# NOTE(sen) Should be empty
earliest_priors_05 %>% filter(.by = c(pid), n() > 1)

mutliyear_h3_egg_plot <- serology_h3_egg %>%
    left_join(earliest_priors_05, "pid") %>%
    filter(!is.na(prior_value), earliest_prior_year <= year) %>%
    summarise(.by = c(year, day, prior_value, earliest_prior_year), summarise_logmean(titre)) %>%
    mutate(
        xcoord = as.integer(factor(day)) + ((year - 2020) * 3), 
        earliest_prior_year_fac = factor(earliest_prior_year),
        xcoord = xcoord + (as.integer(earliest_prior_year_fac) - 2) * 0.2,
    ) %>%
    ggplot(aes(xcoord, mean, col = earliest_prior_year_fac, shape = earliest_prior_year_fac, lty = earliest_prior_year_fac)) +
    theme_bw() +
    theme(
        panel.spacing = unit(0, "null"),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1),
    ) +
    facet_wrap(~prior_value, ncol = 1, strip.position = "right", labeller = function(breaks) mutate(breaks, prior_value = paste(prior_value, "prior"))) +
    scale_x_continuous(
        "Serology year/day", 
        breaks = 1:9, 
        labels = expand.grid(day = c(0, 14, 220), year = 2020:2022) %>% mutate(str = paste0(year, " d", day)) %>% pull(str)
    ) +
    scale_y_log10("Titre", breaks = 5 * 2^(0:15)) +
    scale_color_manual("Earliest prior year", values = c("#00964b", "#9e21d5", "#5d8cfa")) +
    scale_shape_discrete("Earliest prior year") +
    scale_linetype_manual("Earliest prior year", values = c("solid", "31", "11")) +
    geom_line(aes(group = earliest_prior_year)) +
    geom_pointrange(aes(ymin = low, ymax = high))

ggsave("multiyear-titres/mutiyear-titres-h3-egg.pdf", mutliyear_h3_egg_plot, width = 10, height = 10, units = "cm")
ggsave("multiyear-titres/mutiyear-titres-h3-egg.png", mutliyear_h3_egg_plot, width = 10, height = 10, units = "cm")

