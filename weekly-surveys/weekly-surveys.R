library(tidyverse)
library(furrr)

plan(multisession)

redcap_tokens <- read_csv("data-raw/redcap-tokens.csv", col_types = cols())

redcap_request <- function(project_year, event, fields, ...) {
  httr::POST(
    "https://biredcap.mh.org.au/api/",
    body = list(
      token = redcap_tokens %>% filter(year == project_year) %>% pull(token),
      content = "record",
      events = event,
      fields = fields,
      format = "csv",
      exportDataAccessGroups = "true",
      overwriteBehavior = "overwrite",
      ...
    )
  ) %>%
    httr::content(as = "text") %>%
    read_csv(col_types = cols()) %>%
    mutate(redcap_project_year = project_year)
}

redcap_upload <- function(project_year, event, data, ...) {
  temp_file <- tempfile()
  if (event != "") {
    data <- data %>% mutate(redcap_event_name = event)
  }
  write_csv(data, temp_file, na = "")
  csv_string <- readChar(temp_file, file.info(temp_file)$size)
  httr::POST(
    "https://biredcap.mh.org.au/api/",
    body = list(
      token = redcap_tokens %>% filter(year == project_year) %>% pull(token),
      content = "record",
      fields = paste(colnames(data), collapse = ","),
      format = "csv",
      data = csv_string,
      ...
    )
  ) %>%
    httr::content(as = "text")
}

# redcap can't handle "a lot of" data all at once, so here we are
redcap_upload_spoonfed <- function(n_groups, project_year, event, data) {
  data_split <- data %>%
    mutate(temp_split_var = rep(1:n_groups, length.out = n())) %>%
    group_split(temp_split_var, .keep = FALSE)
  future_map(data_split, ~ redcap_upload(project_year, event, .x))
}

weekly_survey_fields <- c(
  "date_symptom_survey",
  "recent_covax",
  "covax_rec",
  "covax_rec_other",
  "covax_dose",
  "covax_date",
  "covax_batch",
  "systemic",
  "respiratory",
  "fever",
  "chills",
  "headache",
  "myalgia",
  "malaise",
  "n_systemic",
  "cough",
  "sorethroat",
  "runnynose",
  "chestpain",
  "breathing",
  "n_respiratory",
  "ari_definition",
  "symptom_duration",
  "pt_contact",
  "nonpt_contact",
  "absence",
  "duration_absent",
  "health_advice",
  "medical_service_v2",
  "diagnosis_v2",
  "new_illness",
  "new_illness_desc",
  "week_surv_gen_com",
  "weekly_symptom_survey_complete"
)

weekly_surveys_6_back <- redcap_request(
  2022,
  "weekly_survey_6_arm_1",
  paste(c("record_id", weekly_survey_fields), collapse = ",")
)

colnames(weekly_surveys_6_back)
table(weekly_surveys_6_back$weekly_symptom_survey_complete)

weekly_surveys_6_back %>% filter(!is.na(date_symptom_survey))

weekly_surveys6_empty <- weekly_surveys_6_back %>%
  select(-contains("redcap")) %>%
  mutate(across(-record_id, ~NA)) %>% 
  mutate(weekly_symptom_survey_complete = 0)

redcap_upload_spoonfed(
  10, 2022, "weekly_survey_6_arm_1", weekly_surveys6_empty
)