# Look at survey completions

library(tidyverse)
library(hcwstudyapp)

survey_completion_dir <- here::here("survey-completion")

# Functions ===================================================================

plot_survey_completion <- function(data,
                                   first_monday = lubridate::ymd(
                                     "2020-04-13"
                                   )) {
  plot <- data %>%
    ggplot(aes(date_symptom_survey, pid)) +
    ggdark::dark_theme_bw(verbose = FALSE) +
    scale_x_date("Date of survey completion", breaks = "1 week") +
    scale_y_discrete("PID") +
    geom_vline(
      data = tibble(monday_date = seq(
        first_monday, max(data$date_symptom_survey),
        by = "weeks"
      )),
      aes(xintercept = monday_date)
    ) +
    geom_point()
  attr(plot, "nrow") <- nrow(data)
  plot
}

save_plots <- function(plot, name) {
  ggdark::ggsave_dark(
    file.path(survey_completion_dir, paste0(name, ".pdf")), plot,
    width = 20, height = 0.1 * attr(plot, "nrow"), units = "cm",
    limitsize = FALSE
  )
}

find_monday <- function(dates) {
  wday(dates, week_start = 1) <- 1
  dates
}

listcol_to_readable <- function(listcol) {
  map_chr(listcol, ~ paste(.x, collapse = " "))
}

# Script ======================================================================

# all_dat <- down_trans_redcap(readLines("token"))
surveys <- all_dat$symptom %>% select(record_id, date_symptom_survey)
subjects <- all_dat$participant %>%
  select(record_id, pid, site_name, date_screening)

all_subject_surveys <- left_join(subjects, surveys, "record_id")

atleast1_completed <- all_subject_surveys %>%
  filter(!is.na(date_symptom_survey))
atleast1_completed_bysite <- group_split(atleast1_completed, site_name)
names(atleast1_completed_bysite) <-
  group_keys(atleast1_completed, site_name)$site_name

all_subsets <- c(list(all = atleast1_completed), atleast1_completed_bysite)

all_subsets_plot <- map(all_subsets, plot_survey_completion)

iwalk(all_subsets_plot, save_plots)


weekly_survey_reference <- tibble(
  week_index = 1:30,
  date_monday = seq(ymd("2020-04-06"), by = "weeks", length.out = 30)
)

current_week <- weekly_survey_reference %>%
  filter(date_monday < Sys.Date()) %>%
  pull(week_index) %>%
  max()

survey_comp_table <- atleast1_completed %>%
  mutate(date_monday = find_monday(date_symptom_survey)) %>%
  left_join(weekly_survey_reference, "date_monday") %>%
  rename(survey_week = week_index) %>%
  mutate(date_monday = find_monday(date_screening)) %>%
  left_join(weekly_survey_reference, "date_monday") %>%
  rename(screening_week = week_index) %>%
  mutate(screening_week = replace_na(screening_week, 0L)) %>%
  group_by(record_id, pid, site_name, screening_week) %>%
  summarise(completed_weeks = list(survey_week)) %>%
  ungroup() %>%
  mutate(
    should_complete = map(screening_week, ~ seq(.x + 1, current_week)),
    missing_surveys = map2(
      should_complete, completed_weeks, ~ .x[!.x %in% .y]
    )
  ) %>%
  mutate(
    completed_weeks_read = listcol_to_readable(completed_weeks),
    should_complete_read = listcol_to_readable(should_complete),
    missing_surveys_read = listcol_to_readable(missing_surveys)
  )
write_csv(
  select_if(survey_comp_table, ~ !is.list(.)),
  file.path(survey_completion_dir, "survey-completion-table.csv")
)
