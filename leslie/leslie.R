library(tidyverse)

participants <- read_csv("data/participants.csv", col_types = cols())

participants %>% count(recruitment_year)

serology <- read_csv("data/serology.csv", col_types = cols())
bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())

serology %>% filter(year == 2022) %>% summarise(n = length(unique(pid)))
serology %>% filter(year == 2022) %>% select(pid, year, day, vax_inf) %>% distinct() %>% count(vax_inf)

withdrawn <- read_csv("data/withdrawn.csv", col_types = cols())

withdrawn %>% summarise(n = length(unique(pid)))
withdrawn %>% summarise(.by = withdrawn_reentered, n = length(unique(pid)))

withdrawn %>% filter(is.na(withdrawal_date))

serology %>% 
    bind_rows(bleed_dates) %>%
    left_join(participants %>% select(pid, recruitment_year), "pid") %>%
    summarise(.by = c(year, recruitment_year), pids = length(unique(pid)))

contributed_blood_in_2022 <- serology %>% 
    bind_rows(bleed_dates) %>%
    filter(year == 2022) %>% 
    pull(pid) %>% 
    unique()
no2022blood <- setdiff(participants$pid, contributed_blood_in_2022)

withdrawn_for_sure <- withdrawn %>% 
    group_by(pid) %>% 
    filter(withdrawal_date == max(withdrawal_date)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    pull(pid)

setdiff(no2022blood, withdrawn_for_sure) %>% length()

swabs <- read_csv("data/swabs.csv", col_types = cols())

swabs_complete <- swabs %>% 
    group_by(pid, year, postinf_instance) %>%
    filter(sum(swab_result) > 0) %>%
    ungroup() %>%
    filter(!is.na(samp_date))
    
swabs_complete %>%
    select(pid, year, samp_date) %>%
    distinct() %>%
    mutate(samp_date_year = year(samp_date)) %>%
    summarise(.by = samp_date_year, n = n(), pids = length(unique(pid)))

swabs_complete %>%
    filter(str_detect(swab_virus, "^Flu"), swab_result == 1) %>%
    select(pid, year, samp_date) %>%
    distinct() %>%
    mutate(samp_date_year = year(samp_date)) %>%
    summarise(.by = samp_date_year, n = n(), pids = length(unique(pid)))

swabs_complete %>%
    filter(swab_virus == "SARS-CoV-2", swab_result == 1) %>%
    select(pid, year, samp_date) %>%
    distinct() %>%
    mutate(samp_date_year = year(samp_date)) %>%
    summarise(n = n(), pids = length(unique(pid)), min_date = min(samp_date), max_date = max(samp_date))

