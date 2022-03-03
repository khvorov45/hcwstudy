library(tidyverse)

redcap_tokens <- read_csv("pull-redcap/redcap-tokens.csv")

redcap_request_all_data <- function(project_year) {
  httr::POST(
    "https://biredcap.mh.org.au/api/",
    body = list(
      token = redcap_tokens %>% filter(year == project_year) %>% pull(token),
      content = "record",
      format = "csv",
      exportDataAccessGroups = "true"
    )
  ) %>%
    httr::content(as = "text") %>%
    read_csv(col_types = cols(), guess_max = 1e5) %>%
    mutate(across(everything(), as.character)) %>% 
    mutate(redcap_project_year = project_year)
}

all_data_2020 <- redcap_request_all_data(2020)
all_data_2021 <- redcap_request_all_data(2021)
all_data_2022 <- redcap_request_all_data(2022)

all_data <- bind_rows(
	all_data_2020,
	all_data_2021,
	all_data_2022,
)

# NOTE(sen) Construct the individual tables

baseline <- all_data %>%
	filter(redcap_event_name == "baseline_arm_1") %>% 
	select(
		record_id, redcap_data_access_group, pid, redcap_project_year, recruit_year,
		consent, study_group_vacc, consent_unvacc,
		date_baseline_blood, date_7d_blood, date_14d_blood, date_end_season_blood,
		covax_d0_sampdate, covax_d7_sampdate, covax_d14_sampdate,
	)

weekly_survey_fields <- c(
  "date_symptom_survey",
  # "recent_covax",
  # "covax_rec",
  # "covax_rec_other",
  # "covax_dose",
  # "covax_date",
  # "covax_batch",
  # "systemic",
  # "respiratory",
  # "fever",
  # "chills",
  # "headache",
  # "myalgia",
  # "malaise",
  # "n_systemic",
  # "cough",
  # "sorethroat",
  # "runnynose",
  # "chestpain",
  # "breathing",
  # "n_respiratory",
  "ari_definition",
  # "symptom_duration",
  # "pt_contact",
  # "nonpt_contact",
  # "absence",
  # "duration_absent",
  # "health_advice",
  # "medical_service_v2", # NOTE(sen) Checkbox
  # "diagnosis_v2",
  # "new_illness",
  # "new_illness_desc",
  # "week_surv_gen_com",
  "weekly_symptom_survey_complete"
)

weekly_surveys <- all_data %>% 
	filter(str_starts(redcap_event_name, "weekly_survey")) %>% 
	mutate(survey_index = str_replace(redcap_event_name, "weekly_survey_(\\d+)_arm_1", "\\1")) %>% 
	select(
		record_id, redcap_data_access_group, redcap_project_year, survey_index,
		all_of(weekly_survey_fields), #contains("medical_service_v2")
	)

withdrawal <- all_data %>% 
	filter(redcap_event_name == "withdrawal_arm_1") %>% 
	select(
		record_id, redcap_data_access_group, redcap_project_year,
		withdrawn
	)

# NOTE(sen) Construct the final data

make_final_data <- function(access_group, particpants, weekly_surveys, withdrawal) {
	if (is.na(access_group)) {
		baseline <- filter(baseline, is.na(redcap_data_access_group))			
		weekly_surveys <- filter(weekly_surveys, is.na(redcap_data_access_group))			
		withdrawal <- filter(withdrawal, is.na(redcap_data_access_group))			
	} else if (access_group != "all_data") {
		baseline <- filter(baseline, redcap_data_access_group == access_group)
		weekly_surveys <- filter(weekly_surveys, redcap_data_access_group == access_group)
		withdrawal <- filter(withdrawal, redcap_data_access_group == access_group)
	}
	list(baseline = baseline, weekly_surveys = weekly_surveys, withdrawal = withdrawal)
}

access_groups <- c("all_data", unique(all_data$redcap_data_access_group))
names(access_groups) <- access_groups

final_data <- map(access_groups, make_final_data, baseline, weekly_surveys, withdrawal)

iwalk(final_data, ~ write(jsonlite::toJSON(.x), paste0("pull-redcap/redcap-", .y, ".json")))
