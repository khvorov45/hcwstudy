library(tidyverse)

participants <- read_csv("data/participants.csv", col_types = cols())
consent <- read_csv("data/consent.csv", col_types = cols())
swabs <- read_csv("data/swabs.csv", col_types = cols())
surveys <- read_csv("data/weekly-surveys.csv", col_types = cols())
postinf_bleed_dates <- read_csv("data/postinf-bleed-dates.csv", col_types = cols())
bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())
covid_bleed_dates <- read_csv("data/covid-bleed-dates.csv", col_types = cols())
covid_vax <- read_csv("data/covid-vax.csv", col_types = cols())

label_consecutive <- function(values) {
    result = c()
    next_label = 0
    current_index = 1
    next_is_the_same_seq = FALSE
    while (current_index <= length(values)) {
        if (current_index < length(values)) {
            this_value = values[current_index]
            next_value = values[current_index + 1]
            next_is_the_same_seq = next_value - this_value == 1
        }
        result = c(result, next_label)
        if (!next_is_the_same_seq) {
            next_label = next_label + 1
        }
        current_index = current_index + 1
    }
    result
}

all_covid_infections <- swabs %>%
    filter(swab_virus == "SARS-CoV-2", swab_result == 1) %>%
    select(pid, year, postinf_instance, samp_date)

all_covid_infections %>%
    left_join(
        postinf_bleed_dates %>%
            summarise(.by = c(pid, year, postinf_instance), bled = any(!is.na(bleed_date)), bleed_dates = paste(sort(bleed_date), collapse = ",")),
        c("pid", "year", "postinf_instance")
    ) %>%
    select(-postinf_instance) %>%
    filter(.by = c(pid, year, samp_date), n() == 1 | !is.na(bled)) %>%
    group_by(pid) %>%
    arrange(samp_date) %>%
    mutate(
        weeks_from_start = week(samp_date) + 52 * (year(samp_date) - 2020),
        event_index = label_consecutive(weeks_from_start),
    ) %>%
    ungroup() %>%
    summarise(.by = c(pid, event_index), bled = any(!is.na(bled)), swab_date = first(samp_date), bleed_dates = first(bleed_dates)) %>%
    select(-event_index) %>%
    left_join(participants %>% select(pid, site), "pid") %>%
    select(pid, site, swab_date, bled, bleed_dates) %>%
    arrange(pid) %>%
    write_csv("leslie/covid-infection-bleed-followup.csv")

all_postvax_pbmcs <- bleed_dates %>%
    filter(samp_type == "pbmc" | samp_type == "both") %>%
    bind_rows(covid_bleed_dates %>% filter(str_detect(samp_type, "pbmc"))) %>%
    filter(day %in% c(7, 14)) %>%
    select(-day, -samp_type)

covid_vax %>%
    # NOTE(sen) Only the ones who consented to the nested study
    inner_join(
        consent %>%
            filter(disease == "covid", consent == "nested") %>%
            select("pid") %>%
            distinct(),
        "pid"
    ) %>%
    select(pid, dose, vax_date = date) %>%
    left_join(all_postvax_pbmcs %>% select(pid, postvax_pbmc_bleed_date = date), "pid", relationship = "many-to-many") %>%
    mutate(close_enough = postvax_pbmc_bleed_date - vax_date <= 7 & postvax_pbmc_bleed_date - vax_date >= 0) %>%
    summarise(.by = c(pid, dose), have_pbmc = any(close_enough)) %>%
    mutate(have_pbmc = replace_na(have_pbmc, FALSE)) %>%
    filter(have_pbmc) %>%
    select(-have_pbmc) %>%
    left_join(participants %>% select(pid, site), "pid") %>%
    select(pid, site, dose) %>%
    arrange(pid) %>%
    write_csv("leslie/covid-vax-have-pbmc.csv")

annette_list <- readxl::read_excel("leslie/Additional Followup of COVID infected.xlsx", range = "C1:D75")

annette_list %>%
    mutate(pid = recode(PID, "ALF-092" = "ALF-819", "JHH-830" = "JHH-082")) %>%
    left_join(
        read_csv("data/yearly-changes.csv", col_types = cols()) %>%
            filter(redcap_project_year == 2022) %>%
            select(pid, redcap_project_year, pid_og), c("pid" = "pid")
    ) %>%
    group_by(Site) %>%
    group_map(function(data, key) {
        paste0("[baseline_arm_1][pid]='", data$pid_og, "'", collapse = " or\n")
    }) %>%
    paste(collapse = "\n\n") %>%
    cat()
