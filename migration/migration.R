# Migrating data from one project year to another

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
      ...
    )
  ) %>%
    httr::content(as = "text") %>%
    read_csv(col_types = cols(), guess_max = 1e5) %>%
    mutate(redcap_project_year = project_year)
}

redcap_upload <- function(project_year, event, data) {
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
      data = csv_string
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

#
# SECTION Withdrawal
#

redcap_withdrawal_request <- function(project_year) {
  redcap_request(
    project_year,
    "withdrawal_arm_1",
    "record_id,withdrawn",
    exportDataAccessGroups = "true",
    rawOrLabel = "raw"
  )
}

withdrawn2020 <- redcap_withdrawal_request(2020) %>% filter(withdrawn == 1)
withdrawn2021 <- redcap_withdrawal_request(2021) %>% filter(withdrawn == 1)
withdrawn2022 <- redcap_withdrawal_request(2022) %>% filter(withdrawn == 1)
withdrawn2023 <- redcap_withdrawal_request(2023) %>% filter(withdrawn == 1)

#
# SECTION Consent
#

redcap_consent_request <- function(project_year) {
  names <- c(
    "record_id",
    "consent",
    "consent_date",
    "add_bleed",
    "consent_future_use",
  
    "study_group_vacc",
    "firstname_vacc",
    "surname_vacc",
    "econsent_date_vacc",
    "econsent_future_vacc",
    "esignature_vacc",
    "sitestaff_name",
    "sitestaff_signature",
    "date_site_sign_v",

    "consent_unvacc",
    "firstname_unvacc",
    "surname_unvacc",
    "econsent_date_unvacc",
    "econsent_future_unvacc",
    "esignature_unvacc",
    "sitestaff_name_uv",
    "sitestaff_signature_uv",
    "date_site_sign_uv"
  )
  redcap_request(
    project_year,
    "baseline_arm_1",
    exportDataAccessGroups = "true",
    paste0(names, collapse = ","),
    rawOrLabel = "raw"
  )
}

consent2020 <- redcap_consent_request(2020)
consent2021 <- redcap_consent_request(2021)
consent2022 <- redcap_consent_request(2022)
consent2023 <- redcap_consent_request(2023)

active2020 <- consent2020 %>%
  select(-contains("redcap")) %>%
  filter(consent == 1 | !is.na(study_group_vacc) | consent_unvacc == 1) %>%
  filter(!record_id %in% withdrawn2020$record_id)

active2021 <- consent2021 %>%
  select(-contains("redcap")) %>%
  filter(consent == 1 | !is.na(study_group_vacc) | consent_unvacc == 1) %>%
  filter(!record_id %in% withdrawn2021$record_id)

active2022 <- consent2022 %>%
  select(-contains("redcap")) %>%
  filter(consent == 1 | !is.na(study_group_vacc) | consent_unvacc == 1) %>%
  filter(!record_id %in% withdrawn2022$record_id)

active2023 <- consent2023 %>%
  select(-contains("redcap")) %>%
  filter(consent == 1 | !is.na(study_group_vacc) | consent_unvacc == 1) %>%
  filter(!record_id %in% withdrawn2022$record_id)

# redcap_upload_spoonfed(20, 2023, "baseline_arm_1", active2022)

data_access_groups2021 <- consent2021 %>%
  select(record_id, redcap_data_access_group) %>%
  filter(record_id %in% active2021$record_id)

data_access_groups2022 <- consent2022 %>%
  select(record_id, redcap_data_access_group) %>%
  filter(record_id %in% active2022$record_id)

data_access_groups2023 <- consent2023 %>%
  select(record_id, redcap_data_access_group) %>%
  filter(record_id %in% active2023$record_id)

# NOTE(sen) If data access group doesn't get assigned, the record will be invisible
# unless you have full access.

# redcap_upload_spoonfed(20, 2023, "", data_access_groups2022)

# redcap_upload(
#   2023, "baseline_arm_1",
#   consent2022 %>%
#     filter(record_id == "172-19") %>%
#     select(-redcap_project_year)
# )

#
# SECTION PID, study group
#

redcap_pid_request <- function(project_year) {
  redcap_request(
    project_year,
    "baseline_arm_1",
    "record_id,pid,recruit_year,studygroup_y1,studygroup_y2,main_vacc,main_unvacc,nested_naive,nested_infect",
    exportDataAccessGroups = "true",
    rawOrLabel = "raw"
  )
}

pid2021 <- redcap_pid_request(2021)
pid2022 <- redcap_pid_request(2022)

process_pid <- function(data) {
  data %>%
    mutate(
      studygroup_y3 = case_when(
        nested_naive == 1 | nested_infect == 1 ~ 3,
        main_vacc == 1 ~ 1,
        main_unvacc == 1 ~ 2,
      ),
      recruit_year = replace_na(recruit_year, 2022)
    ) %>%
    select(record_id, pid, recruit_year, studygroup_y1, studygroup_y2, studygroup_y3)
}

# redcap_upload(
#   2023, "baseline_arm_1", 
#   pid2022 %>% 
#     filter(record_id == "172-19") %>% 
#     process_pid()
# )

# redcap_upload(
#   2022, "baseline_arm_1", 
#   pid2021 %>% 
#     filter(pid == "JHH-021") %>% 
#     process_pid()
# )

# redcap_upload(
#   2022, "baseline_arm_1", 
#   pid2021 %>% 
#     filter(pid == "ALF-804") %>% 
#     process_pid()
# )

# jhh_806_2021_record_id <- pid2021 %>% filter(pid == "JHH-806") %>% pull(record_id)
# jhh_806_2022_record_id <- pid2022_current %>% filter(pid == "JHH-806") %>% pull(record_id)

# jhh_007_2021_record_id <- pid2021 %>% filter(pid == "JHH-007") %>% pull(record_id)
# jhh_007_2022_record_id <- pid2022_current %>% filter(pid == "JHH-007") %>% pull(record_id)

# pch_043_2021_record_id <- pid2021 %>% filter(pid == "PCH-043") %>% pull(record_id)
# pch_043_2022_record_id <- pid2022_current %>% filter(pid == "PCH-043") %>% pull(record_id)

# pch_096_2021_record_id <- pid2021 %>% filter(pid == "PCH-096") %>% pull(record_id)
# pch_096_2022_record_id <- pid2022_current %>% filter(pid == "PCH-096") %>% pull(record_id)

#
# SECTION Screening
#

redcap_screening_request <- function(project_year) {
  redcap_request(
    project_year,
    "baseline_arm_1",
    "record_id,date_screening,site_name,screening_method,screening_interest,screening_declinereason,screening_declineother,screening_age,screening_employee,screening_bloodsamp,screening_followup,screening_phone,screening_recent_rx,screening_ill,vac_2020,vac_2019,vac_2018,vac_2017,vac_2016,vac_2015,screening_vacc,assent,data_entry,screening_covidvacc,email,mobile_number",
    exportDataAccessGroups = "true",
    rawOrLabel = "raw"
  )
}

screening2021 <- redcap_screening_request(2021)
screening2022 <- redcap_screening_request(2022)

screening2022_active <- screening2022 %>%
  filter(record_id %in% active2022$record_id) %>%
  filter(!is.na(date_screening) | !is.na(site_name)) %>%
  select(-contains("redcap"))

# redcap_upload_spoonfed(20, 2023, "baseline_arm_1", screening2022_active)

# redcap_upload(
#   2023,
#   "baseline_arm_1",
#   screening2022 %>%
#     filter(record_id == "172-19") %>%
#     filter(!is.na(date_screening) | !is.na(site_name)) %>%
#     select(-contains("redcap"))
# )

# redcap_upload(
#   2022,
#   "baseline_arm_1",
#   screening2021 %>%
#     filter(record_id == "170-31") %>%
#     filter(!is.na(date_screening) | !is.na(site_name)) %>%
#     select(-contains("redcap"))
# )

# redcap_upload(
#   2022,
#   "baseline_arm_1",
#   screening2021 %>%
#     filter(record_id == "168-4") %>%
#     filter(!is.na(date_screening) | !is.na(site_name)) %>%
#     select(-contains("redcap"))
# )

#
# SECTION Baseline questionnaire
#

redcap_baseline_request <- function(project_year) {
  redcap_request(
    project_year,
    "baseline_arm_1",
    "record_id,baseline_q_date,a1_gender,a2_dob,a3_atsi,a4_children,a5_height,a6_weight,b1_medicalhx,c1_yrs_employed,c2_emp_status,c3_occupation,c3_spec,c4_workdept,c4_spec,c5_clin_care,d1_future_vacc",
    exportDataAccessGroups = "true",
    rawOrLabel = "raw"
  )
}

baseline2021 <- redcap_baseline_request(2021)
baseline2022 <- redcap_baseline_request(2022)

process_baseline <- function(data) {
  data %>%
    select(-contains("redcap"))
}

baseline2022_active <- baseline2022 %>%
  filter(record_id %in% active2022$record_id, !is.na(baseline_q_date)) %>%
  process_baseline()

# redcap_upload_spoonfed(10, 2023, "baseline_arm_1", baseline2022_active)

# redcap_upload(
#   2023,
#   "baseline_arm_1",
#   baseline2022 %>%
#     filter(record_id == "172-19") %>%
#     process_baseline()
# )

# redcap_upload(
#   2022,
#   "baseline_arm_1",
#   baseline2021 %>%
#     filter(record_id == "170-31") %>%
#     process_baseline()
# )

# redcap_upload(
#   2022,
#   "baseline_arm_1",
#   baseline2021 %>%
#     filter(record_id == "168-4") %>%
#     process_baseline()
# )

# redcap_upload(
#   2022,
#   "baseline_arm_1",
#   baseline2021 %>%
#     filter(record_id == jhh_806_2021_record_id) %>%
#     process_baseline() %>%
#     mutate(record_id = jhh_806_2022_record_id)
# )

# redcap_upload(
#   2022,
#   "baseline_arm_1",
#   baseline2021 %>%
#     filter(record_id == jhh_007_2021_record_id) %>%
#     process_baseline() %>%
#     mutate(record_id = jhh_007_2022_record_id)
# )

# redcap_upload(
#   2022,
#   "baseline_arm_1",
#   baseline2021 %>%
#     filter(record_id == pch_043_2021_record_id) %>%
#     process_baseline() %>%
#     mutate(record_id = pch_043_2022_record_id)
# )

#
# SECTION Covid vaccines
#

redcap_covax_request <- function(project_year) {
  redcap_request(
    project_year,
    "vaccination_arm_1",
    "record_id,covid_vac_brand,other_covax_brand,covid_vac_dose1_rec,covid_vacc_date1,covid_vac_batch1,covid_vac_brand2,other_covax_brand2,covid_vac_dose2_rec,covid_vacc_date2,covid_vac_batch2,covid_vac_brand3,other_covax_brand3,covid_vac_dose3_rec,covid_vacc_date3,covid_vac_batch3,covid_vac_brand4,other_covax_brand4,covid_vac_dose4_rec,covid_vacc_date4,covid_vac_batch4",
    exportDataAccessGroups = "true",
    rawOrLabel = "raw"
  )
}

covax2021 <- redcap_covax_request(2021)
covax2022 <- redcap_covax_request(2022)

covax2022_active <- covax2022 %>%
  select(-contains("redcap")) %>%
  filter(record_id %in% active2022$record_id)

# redcap_upload_spoonfed(10, 2023, "vaccination_arm_1", covax2022_active)

# redcap_upload(
#   2023,
#   "vaccination_arm_1",
#   covax2022 %>%
#     select(-contains("redcap")) %>%
#     filter(record_id == "172-19")
# )

# redcap_upload(
#   2022,
#   "vaccination_arm_1",
#   covax2021 %>%
#     select(-contains("redcap")) %>%
#     filter(record_id == "170-31")
# )

# redcap_upload(
#   2022,
#   "vaccination_arm_1",
#   covax2021 %>%
#     select(-contains("redcap")) %>%
#     filter(record_id == "168-4")
# )
