library(tidyverse)

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
      ...
    )
  ) %>%
    httr::content(as = "text") %>%
    read_csv(col_types = cols()) %>%
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
      data = csv_string,
      overwriteBehavior = "overwrite"
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

social_contacts_fields1 <- c(
  "contact", "age_contact", "gender_contact", "interaction_contact", "location_contact",
  "known_contact", "add_contact"
)

social_contancts_fields_all <- map(1:50, ~paste0(social_contacts_fields1, .x)) %>% flatten() %>% as.character()

social_contacts2022 <- redcap_request(
  2022, "baseline_arm_1", 
  paste0("record_id,", paste(social_contancts_fields_all, collapse = ","))
)

social_contacts2022 %>%
  filter(redcap_data_access_group == "sydney") %>%
  select(-contains("redcap"))

consent_vacc2022 <- redcap_request(
  2022, 
  "baseline_arm_1", 
  "record_id,study_group_vacc,firstname_vacc,surname_vacc,econsent_date_vacc,econsent_future_vacc,esignature_vacc,sitestaff_name,sitestaff_signature,date_site_sign_v,econsent_form_vaccinated_complete"
)

consent_vacc2022_cleared <- consent_vacc2022 %>%
  filter(redcap_data_access_group == "sydney") %>%
  filter(row_number() == 1) %>%
  select(-contains("redcap")) %>%
  mutate(across(c(-record_id, -study_group_vacc), function(x) NA)) %>%
  mutate(econsent_form_vaccinated_complete = 0)

#redcap_upload(2022, "baseline_arm_1", consent_vacc2022_cleared)

#
# SECTION Upload serology data
#

read_csv("data/serology.csv", col_types = cols())
