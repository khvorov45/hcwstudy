library(tidyverse)

svnt <- read_csv("postinf-sample/sVNTanalysis.csv", col_types = cols())

bled_appropriately_after_covid_vax <- svnt %>%
    filter(TP == "post") %>%
    mutate(
        all_brands = paste(Covax1_brand, Covax2_brand, Covax3_brand, Covax4_brand, sep = ",")
    ) %>%
    select(
        pid = PID, postvax_bleed_date = SampleDate, all_brands
    )

# NOTE(sen) Check there is one post bleed per individual
bled_appropriately_after_covid_vax %>% group_by(pid) %>% filter(n() > 1)

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

bled_after_covid_vax_and_infected <- postinf_covid_bleeds %>%
    inner_join(bled_appropriately_after_covid_vax, "pid") %>%
    rename(postinf_bleed_date = bleed_date) %>%
    filter(postvax_bleed_date < swab_date) %>%
    mutate(postinf_bleed_day = paste0("postinf_bleed_day", postinf_bleed_day)) %>%
    pivot_wider(names_from = "postinf_bleed_day", values_from = "postinf_bleed_date") %>%
    arrange(pid) %>%
    select(
        pid, year, all_brands, covid_pos_swab = swab_date, postvax_bleed_date,
        postinf_bleed_day7, postinf_bleed_day14, postinf_bleed_day30,
    ) %>%
    left_join(participants, "pid")

bled_and_infected_relevant_categories <- bled_after_covid_vax_and_infected %>%
    count(all_brands, gender, age_group, name = "category_count") %>%
    mutate(category_index = row_number())

write_csv(
    bled_after_covid_vax_and_infected %>% 
        inner_join(bled_and_infected_relevant_categories, c("all_brands", "gender", "age_group")) %>%
        arrange(pid), 
    "postinf-sample/bled_and_infected.csv"
)

bled_after_covid_vax_and_not_infected_all <- bled_appropriately_after_covid_vax %>%
    filter(!pid %in% bled_after_covid_vax_and_infected$pid) %>%
    left_join(participants, "pid") %>%
    left_join(bled_and_infected_relevant_categories, c("all_brands", "gender", "age_group"))

resample <- function(x, ...) x[sample.int(length(x), ...)]

bled_after_covid_vax_and_not_infected_all %>% filter(all_brands == "AZ,AZ,AZ,NA")
bled_after_covid_vax_and_not_infected_all %>% count(all_brands)

set.seed(1)
bled_after_covid_vax_and_not_infected_matched <- bled_after_covid_vax_and_not_infected_all %>%
    filter(!is.na(category_index)) %>%
    # NOTE(sen) Make sure each pid appears in only one category
    group_by(pid) %>%
    filter(category_index == resample(category_index, 1)) %>%
    group_by(category_index) %>%
    filter(row_number() %in% sample(1:n(), unique(category_count))) %>%
    ungroup() %>%
    bind_rows(bled_after_covid_vax_and_not_infected_all %>% filter(all_brands == "AZ,AZ,AZ,NA")) %>%
    arrange(pid)

write_csv(bled_after_covid_vax_and_not_infected_matched, "postinf-sample/bled_after_and_not_infected_matched.csv")
