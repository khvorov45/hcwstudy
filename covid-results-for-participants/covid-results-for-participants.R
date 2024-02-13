library(tidyverse)
library(officer)
library(furrr)

plan(multisession)

serology_covid <- read_csv("data/serology-covid.csv", col_types = cols()) %>%
    filter(strain == "Wuhan", vax_inf == "V", bleed_day_id %in% c(0, 14, 220)) %>%
    select(-strain, -vax_inf)

covid_vax <- read_csv("data/covid-vax.csv", col_types = cols())
covid_bleed_dates <- read_csv("data/covid-bleed-dates.csv", col_types = cols())
flu_bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols()) %>% select(-samp_type) %>% distinct()

# NOTE(sen) Should be empty
serology_covid %>% filter(.by = c(pid, bleed_day_id), n() > 1)

output_dirs <- paste0("covid-results-for-participants/", unique(str_sub(serology_covid$pid, 1, 3)))
walk(output_dirs, unlink, recursive = TRUE)
walk(output_dirs, dir.create)

today_date <- as.character(Sys.Date())

relevant_covid_bleed_dates <- covid_bleed_dates %>% filter(year < 2023, day %in% c(0, 14)) %>% select(pid, bleed_day_id = day, date) %>% distinct()

# NOTE(sen) Should be empty
relevant_covid_bleed_dates %>% filter(.by = c(pid, bleed_day_id), n() > 1)

temppid <- "WCH-806"
serology_covid %>% filter(pid == temppid)
covid_bleed_dates %>% filter(pid == temppid)
flu_bleed_dates %>% filter(pid == temppid)
covid_vax %>% filter(pid == temppid)

serology_covid %>%
    left_join(covid_vax %>% filter(dose == 2) %>% select(pid, brand), "pid") %>%
    (function(data) {
        bind_rows(
            data %>% filter(bleed_flu_covid == "C") %>% left_join(relevant_covid_bleed_dates, c("pid", "bleed_day_id")),
            data %>% 
                filter(bleed_flu_covid == "F") %>% 
                left_join(
                    # NOTE(sen) Flu bleeds from 2020 that appear in covid serology should all be end-of-season 2020
                    # flu bleeds used as baseline for covid responses 
                    flu_bleed_dates %>%
                        filter(year == 2021 | (year == 2020 & day == 220)) %>%
                        mutate(day = if_else(year == 2021, day, 0)),
                    c("pid", "bleed_year" = "year", "bleed_day_id" = "day")
                )
        )
    }) %>%
    mutate(date = replace_na(as.character(date), ""), ic50 = replace_na(as.character(round(ic50)), "")) %>%
    group_by(pid, brand) %>%
    group_split() %>%
    future_walk(function(group) {
        key <- group %>% select(pid, brand) %>% distinct()
        pid_first3 <- str_sub(key$pid, 1, 3)

        len0ToEmptyStr <- function(arr) if (length(arr) == 0) "" else arr
        pid_doc <- read_docx(glue::glue("covid-results-for-participants/{pid_first3}.docx")) %>%
            body_replace_text_at_bkm("PID", key$pid) %>%
			body_replace_text_at_bkm("REPORTDATE", today_date) %>%
			body_replace_text_at_bkm("PREDATE", group %>% filter(bleed_day_id == 0) %>% pull(date) %>% len0ToEmptyStr()) %>%
			body_replace_text_at_bkm("POSTVAXDATE", group %>% filter(bleed_day_id == 14) %>% pull(date) %>% len0ToEmptyStr()) %>%
			body_replace_text_at_bkm("ENDDATE", group %>% filter(bleed_day_id == 220) %>% pull(date) %>% len0ToEmptyStr()) %>%
            body_replace_text_at_bkm("PRETITRE", group %>% filter(bleed_day_id == 0) %>% pull(ic50) %>% len0ToEmptyStr()) %>%
			body_replace_text_at_bkm("POSTVAXTITRE", group %>% filter(bleed_day_id == 14) %>% pull(ic50) %>% len0ToEmptyStr()) %>%
			body_replace_text_at_bkm("ENDTITRE", group %>% filter(bleed_day_id == 220) %>% pull(ic50) %>% len0ToEmptyStr()) %>%
			body_replace_text_at_bkm("VACCINENAME", key$brand)

        filename <- file.path("covid-results-for-participants", paste0(pid_first3), paste0(key$pid, ".docx"))
        print(pid_doc, target = filename)
    })

system("sudo covid-results-for-participants/topdf.sh")
