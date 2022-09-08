library(tidyverse)

bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())

find_nearest_bleed_date_after <- function(pid_, vaccine_date_) {
    all_dates_after <- bleed_dates %>%
        filter(pid == pid_, date >= vaccine_date_) %>%
        pull(date)

    if (length(all_dates_after) == 0) {
        NA
    } else {
        min(all_dates_after)
    }
}

covid_vax <- read_csv("data/covid-vax.csv", col_types = cols()) %>%
    group_by(pid, dose) %>%
    mutate(postvax_bleed_date = find_nearest_bleed_date_after(pid, date)) %>%
    ungroup() %>%
    group_by(pid) %>%
    arrange(dose) %>%
    mutate(all_brands = accumulate(brand, ~paste(.x, .y, sep = ","), "")) %>%
    ungroup() %>%
    select(-brand, -batch)

bleed_after_min <- 7
bleed_after_max <- 30
bled_appropriately_after_covid_vax <- covid_vax %>% 
    filter(postvax_bleed_date - date >= bleed_after_min & postvax_bleed_date - date <= bleed_after_max)

swabs_covid <- read_csv("data/swabs.csv", col_types = cols()) %>%
    filter(swab_virus == "SARS-CoV-2" & swab_result == 1) %>%
    select(-swab_virus, -swab_result) %>%
    group_by(pid) %>%
    filter(samp_date == min(samp_date)) %>%
    ungroup()

postinf_covid_bleeds <- read_csv("data/postinf-bleed-dates.csv", col_types = cols()) %>%
    select(-samp_date, -swab_collection) %>%
    inner_join(swabs_covid, c("pid", "year", "postinf_instance")) %>%
    select(-postinf_instance) %>%
    rename(postinf_bleed_day = day, swab_date = samp_date)

age_group_splits <- c(-Inf, 30, 40, 50, 60, Inf)
participants <- read_csv("data/participants.csv", col_types = cols()) %>%
    mutate(age_group = cut(age_screening, age_group_splits)) %>%
    select(-email, -mobile)

min_days_vax_before_pos <- 30
bled_after_covid_vax_and_infected <- postinf_covid_bleeds %>%
    inner_join(bled_appropriately_after_covid_vax, "pid") %>%
    rename(
        vaccine_date = date, 
        postinf_bleed_date = bleed_date, 
    ) %>%
    filter(vaccine_date < swab_date - min_days_vax_before_pos) %>%
    mutate(postinf_bleed_day = paste0("postinf_bleed_day", postinf_bleed_day)) %>%
    pivot_wider(names_from = "postinf_bleed_day", values_from = "postinf_bleed_date") %>%
    arrange(pid, dose) %>%
    select(
        pid, year, all_brands, dose, covid_pos_swab = swab_date, vaccine_date, postvax_bleed_date, 
        postinf_bleed_day7, postinf_bleed_day14, postinf_bleed_day30,
    ) %>%
    group_by(pid) %>%
    filter(dose == max(dose)) %>%
    ungroup() %>%
    left_join(participants, "pid")

bled_after_covid_vax_and_not_infected_all <- bled_appropriately_after_covid_vax %>%
    filter(!pid %in% bled_after_covid_vax_and_infected$pid) %>%
    left_join(participants, "pid")

bled_and_infected_relevant_categories <- bled_after_covid_vax_and_infected %>%
    count(dose, all_brands, gender, age_group, name = "category_count") %>%
    mutate(category_index = row_number())

write_csv(
    bled_after_covid_vax_and_infected %>% 
        inner_join(bled_and_infected_relevant_categories, c("dose", "all_brands", "gender", "age_group")) %>%
        arrange(pid, dose), 
    "postinf-sample/bled_and_infected.csv"
)

resample <- function(x, ...) x[sample.int(length(x), ...)]

set.seed(1)
bled_after_covid_vax_and_not_infected_matched <- bled_after_covid_vax_and_not_infected_all %>%
    inner_join(bled_and_infected_relevant_categories, c("dose", "all_brands", "gender", "age_group")) %>%
    # NOTE(sen) Make sure each pid appears in only one category
    group_by(pid) %>%
    filter(category_index == resample(category_index, 1)) %>%
    group_by(category_index) %>%
    filter(row_number() %in% sample(1:n(), unique(category_count))) %>%
    ungroup() %>%
    arrange(pid, dose)

write_csv(bled_after_covid_vax_and_not_infected_matched, "postinf-sample/bled_after_and_not_infected_matched.csv")
