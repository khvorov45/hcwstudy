# Look at survey completions

library(tidyverse)
library(hcwstudyapp)
library(lubridate)

survey_completion_dir <- here::here("survey-completion")

# Functions ===================================================================

find_monday <- function(dates) {
  wday(dates, week_start = 1) <- 1
  dates
}

listcol_to_readable <- function(listcol) {
  map_chr(listcol, ~ paste(.x, collapse = " "))
}

find_screening_week <- function(data, weekly_survey_reference) {
  data %>%
    mutate(date_monday = find_monday(date_screening)) %>%
    left_join(weekly_survey_reference, "date_monday") %>%
    rename(screening_week = week_index) %>%
    mutate(screening_week = replace_na(screening_week, 0L))
}

gen_should_complete <- function(screening_week_index) {
  latest_week <- weekly_survey_reference %>%
    filter(date_monday < Sys.Date()) %>%
    pull(week_index) %>%
    max() - 1
  map(
    screening_week_index,
    function(i) if (i + 1 > latest_week) integer(0) else seq(i + 1, latest_week)
  )
}

save_csv_table <- function(table, name) {
  write_csv(
    select_if(table, ~ !is.list(.)),
    file.path(survey_completion_dir, paste0(name, ".csv"))
  )
}

# Script ======================================================================

all_dat <- down_trans_redcap(readLines("token"))
surveys <- all_dat$symptom %>% select(record_id, survey_week_index)
subjects <- all_dat$participant %>%
  select(record_id, pid, email, mobile_number, site_name, date_screening)
withdrawals <- all_dat$withdrawal

all_subject_surveys <- left_join(subjects, surveys, "record_id") %>%
  # PID presence is proxy for consent/enrollment
  filter(!is.na(pid), !record_id %in% withdrawals$record_id)

weekly_survey_reference <- tibble(
  week_index = 1:30,
  date_monday = seq(ymd("2020-04-06"), by = "weeks", length.out = 30)
)

survey_comp_table <- all_subject_surveys %>%
  filter(!is.na(survey_week_index)) %>%
  find_screening_week(weekly_survey_reference) %>%
  group_by(record_id, pid, site_name, screening_week) %>%
  summarise(completed_weeks = list(survey_week_index)) %>%
  ungroup() %>%
  mutate(
    should_complete = gen_should_complete(screening_week),
    missing_surveys = map2(should_complete, completed_weeks, setdiff),
    completed_weeks_read = listcol_to_readable(completed_weeks),
    should_complete_read = listcol_to_readable(should_complete),
    missing_surveys_read = listcol_to_readable(missing_surveys)
  )
save_csv_table(survey_comp_table, "survey-completion-table")

all_surveys_miss <- all_subject_surveys %>%
  filter(is.na(survey_week_index)) %>%
  select(-survey_week_index) %>%
  find_screening_week(weekly_survey_reference) %>%
  mutate(
    should_complete = gen_should_complete(screening_week),
    should_complete_read = listcol_to_readable(should_complete)
  )
save_csv_table(all_surveys_miss, "all-surveys-missing-table")
