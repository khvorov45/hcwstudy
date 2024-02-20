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

# social_contacts2022 <- redcap_request(
#   2022, "baseline_arm_1", 
#   paste0("record_id,", paste(social_contancts_fields_all, collapse = ","))
# )

# social_contacts2022 %>%
#   filter(redcap_data_access_group == "sydney") %>%
#   select(-contains("redcap"))

# consent_vacc2022 <- redcap_request(
#   2022, 
#   "baseline_arm_1", 
#   "record_id,study_group_vacc,firstname_vacc,surname_vacc,econsent_date_vacc,econsent_future_vacc,esignature_vacc,sitestaff_name,sitestaff_signature,date_site_sign_v,econsent_form_vaccinated_complete"
# )

# consent_vacc2022_cleared <- consent_vacc2022 %>%
#   filter(redcap_data_access_group == "sydney") %>%
#   filter(row_number() == 1) %>%
#   select(-contains("redcap")) %>%
#   mutate(across(c(-record_id, -study_group_vacc), function(x) NA)) %>%
#   mutate(econsent_form_vaccinated_complete = 0)

#redcap_upload(2022, "baseline_arm_1", consent_vacc2022_cleared)

#
# SECTION Upload serology data
#

create_redcap_serology_var_name <- function(virus, day) {
  str_replace_all(virus, "[/ ]", "_") %>% tolower() %>% paste0("serology_results_day_", day, "_", .)
}

# NOTE(sen) Create redcap HI instruments for each year. Need to upload the zip files manually.
read_csv("data/serology.csv", col_types = cols()) %>%
  select(virus, year) %>%
  distinct() %>%
  mutate(for_joining = 1) %>%
  inner_join(tibble(day = c(0, 7, 14, 220), for_joining = 1), "for_joining", relationship = "many-to-many") %>%
  select(-for_joining) %>%
  mutate(
    `Variable / Field Name` = create_redcap_serology_var_name(virus, day),
    `Form name` = "serology_results",
    `Section Header` = "",
    `Field Type` = "text",
    `Field Label` = paste0(virus, " day ", day),
  ) %>%
  select(-virus, -day) %>%
  group_by(year) %>%
  group_walk(function(data, key) {
    instrument_path <- "redcap-misc/instrument.csv"
    zip_name <- glue::glue("serology_form_{key$year}.zip")
    zip_path <- glue::glue("redcap-misc/{zip_name}")
    if (file.exists(instrument_path)) file.remove(instrument_path)
    if (file.exists(zip_path)) file.remove(zip_path)
    write_csv(data, instrument_path)
    zip::zip(zip_name, "instrument.csv", root = "redcap-misc")
    file.remove(instrument_path)
  })

read_csv("data/serology.csv", col_types = cols()) %>%
  filter(vax_inf == "V") %>%
  mutate(redcap_var_name = create_redcap_serology_var_name(virus, day)) %>%
  select(pid, year, redcap_var_name, titre) %>%
  inner_join(
    read_csv("data/yearly-changes.csv", col_types = cols()) %>%
      select(record_id, pid, redcap_project_year),
    c("pid", "year" = "redcap_project_year"),
    relationship = "many-to-many",
  ) %>%
  select(-pid) %>%
  group_by(year) %>%
  group_walk(function(data, key) {
    data %>%
      pivot_wider(names_from = "redcap_var_name", values_from = "titre") %>%
      print() %>%
      redcap_upload_spoonfed(30, key$year, "baseline_arm_1", .) %>%
      print()
  })
