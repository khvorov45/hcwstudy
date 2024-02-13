library(tidyverse)

# weekly_surveys <- read_csv("data/weekly-surveys.csv", col_types = cols()) %>% mutate(site = substr(pid, 1, 3), surveyid = 1:nrow(.))
# participants <- read_csv("data/participants.csv", col_types = cols())
# comorbidities <- read_csv("data/comorbidities.csv", col_types = cols())
# bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())
vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())
serology <- read_csv("data/serology.csv", col_types = cols())
swabs <- read_csv("data/swabs.csv", col_types = cols())

all_flu_infections <- swabs %>%
    filter(str_starts(swab_virus, "Flu"), swab_result == 1, year == 2022) %>%
    select(pid, samp_date, swab_virus) %>%
    # NOTE(sen) Only look at the first infection
    filter(.by = pid, samp_date == min(samp_date)) %>%
    # NOTE(sen) Collapse multiple results into 1
    summarise(
        .by = pid, 
        samp_date = unique(samp_date),
        swab_virus = (function(swab_virus) {
            # NOTE(sen) Check the assumption that all infections are flu A
            stopifnot(all(str_starts(swab_virus, "Flu A")))
            unique_viruses <- unique(swab_virus)
            if (length(unique_viruses) > 1) {
                # NOTE(sen) If there are multiple results and one of them is unsubtyped then ignore
                # the unsubtyped one
                unique_viruses <- unique_viruses[unique_viruses != "Flu A (unsubtyped)"]
            }
            paste0(unique_viruses, collapse = ",")
        })(swab_virus),
    )

# NOTE(sen) Check the assumption that all infections are flu A
stopifnot(all(str_starts(all_flu_infections$swab_virus, "Flu A")))

# NOTE(sen) For all vaccinated, see how long it took them to get infected 
# in the 2022 season while the vaccine was supposed to be protecting them.
# The unvaccinated are excluded.
# Those who got infected before vaccination or less than 14 days after are also excluded.
# The idea is to look at time-at-risk for the vaccinated while they were protected by the vaccine.
# So the relevant period at risk is 14 days after vaccination to end-of-season.
# Any time outside of that shouldn't contribute to time-at-risk.
time_at_risk_data <- vaccinations %>%
    filter(year == 2022, status %in% c("Australia", "Overseas")) %>%
    select(pid, vaccination_date) %>%
    left_join(all_flu_infections, "pid") %>%
    mutate(
        risk_start = vaccination_date + 14, 
        risk_end = ymd("2022-10-01"),
        estimated_infection_date = samp_date - 7,
        time_to_event = as.integer(if_else(is.na(estimated_infection_date), risk_end - risk_start, estimated_infection_date - risk_start)),
        infected = factor(as.integer(!is.na(estimated_infection_date))),
    ) %>%
    filter(time_to_event > 0) %>%
    left_join(
        # NOTE(sen) Just average all the flu a titres unless we have a better idea
        serology %>%
            filter(year == 2022, subtype %in% c("H1", "H3"), virus_vaccine, day == 14) %>%
            summarise(.by = pid, average_postvax_flua_titre = exp(mean(log(titre)))),
        "pid"
    ) %>%
    filter(!is.na(average_postvax_flua_titre))

time_at_risk_plot <- time_at_risk_data %>%
    ggplot(aes(average_postvax_flua_titre, time_to_event, shape = infected, color = infected)) +
    theme_bw() +
    scale_x_log10(breaks = 5 * 2^(0:15)) +
    geom_point()

ggsave("hadrien/time_at_risk_plot.pdf", width = 15, height = 10, units = "cm")

library(survival)
coxph(Surv(time_to_event, as.integer(infected)) ~ I(log(average_postvax_flua_titre)), data = time_at_risk_data)
