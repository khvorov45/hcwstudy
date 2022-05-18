library(tidyverse)

#
# SECTION Serology
#

#system("data-raw/pull-NIHHCWserol.sh")

# NOTE(sen) Export tables from access, one csv per table
#system("data-raw/export-NIHHCWserol.sh")

serology_all_tables_2020 <- list.files("data-raw", pattern = "HI_", full.names = TRUE) %>%
  map_dfr(function(path) {
    read_csv(path, col_types = cols()) %>%
      mutate(
        year = 2020,
        day = str_replace(Time, "V|v", "") %>% as.integer(),
        path = path
      ) %>%
      select(
        pid = PID, year, day, virus = Target,
        titre = Titer, path
      ) %>%
      # NOTE(sen) The whole point is to have a titre measurement, no titre -
      # don't insert into the table at all
      filter(!is.na(titre))
  })

serology_all_tables_2021 <- list.files("data-raw/2021", full.names = TRUE) %>%
  map_dfr(function(path) {
    dat <- readxl::read_excel(path, guess_max = 1e5)

    # NOTE(sen) Pranked
    if ("VirusDesignation" %in% colnames(dat)) {
      dat <- dat %>% rename(`Virus Designation` = VirusDesignation)
    } else if ("Virus_Designation" %in% colnames(dat)) {
      dat <- dat %>% rename(`Virus Designation` = Virus_Designation)
    }

    dat %>%
      mutate(
        year = 2021,
        path = path
      ) %>%
      select(
        pid = PID, year, virus = `Virus Designation`, day = Timepoint,
        titre = Titer, path
      ) %>%
      # NOTE(sen) The whole point is to have a titre measurement, no titre -
      # don't insert into the table at all
      filter(!is.na(titre))
  })

# NOTE(sen) Let's assume JHH-018's titres are all at V0 (they are missing day)
serology_all_tables_2020_fix_day <- serology_all_tables_2020 %>% mutate(
  day = if_else(pid == "JHH-018", 0L, day)
)

# NOTE(sen) Fix virus names
serology_all_tables_2020_fix_viruses <- serology_all_tables_2020_fix_day %>%
  mutate(
    virus = virus %>%
      # NOTE(sen) Inconsistent left-pad
      str_replace("/(\\d)/", "/0\\1/") %>%
      # NOTE(sen) Let's assume that "c" at the end is not important because it's
      # not in the virus table
      str_replace("c$", "") %>%
      # NOTE(sen) Inconsistent egg vs e
      str_replace("egg$", "e") %>%
      # NOTE(sen) Inconsistent south australia
      str_replace("SouthAustralia|SouthAust", "South Australia") %>%
      # NOTE(sen) Inconsistent IVR format
      str_replace("IVR190", "(IVR-190)") %>%
      str_replace(" \\(IVR-190\\)", "e") %>%
      # NOTE(sen) Of course there's gonna be a space before e
      str_replace(" e$", "e") %>%
      # NOTE(sen) This is just to mess with me personally I guess
      str_replace("SthAust_34_19Cell", "A/South Australia/34/2019"),

    # NOTE(sen) Washington egg doesn't have an e at the end BUT ONLY SOMETIMES
    virus = if_else(
      str_detect(path, "B_Vicegg") & !str_detect(virus, "e$"),
      paste0(virus, "e"),
      virus
    ),

    subtype = case_when(
      str_detect(path, "H1") ~ "H1",
      str_detect(path, "H3") ~ "H3",
      str_detect(path, "Yam") ~ "BYam",
      str_detect(path, "Vic") ~ "BVic",
    )
  )

serology_all_tables_2020_fix_viruses %>%
  count(virus, subtype, path)

# NOTE(sen) WCH-025 became WCH-818 and we seem to have V0 WCH-818 data
serology_all_tables_2020_fix_pids <- serology_all_tables_2020_fix_viruses %>%
  filter(pid != "WCH-025")

# NOTE(sen) Fix virus names
serology_all_tables_2021_fix_viruses <- serology_all_tables_2021 %>%
  mutate(
    virus = virus %>%
      str_replace(" [C|c]ell split", "") %>%
      str_replace(" [E|e]gg split", "") %>%
      str_replace(" IVR Egg", "") %>%
      str_replace(" Volume looks right", "") %>%
      str_replace(" IVR_208", "") %>%
      str_replace(" Cell", "") %>%
      str_trim() %>%
      str_replace_all("_", "/"),

    virus = if_else(str_detect(path, "egg"), paste0(virus, "e"), virus),

    subtype = case_when(
      str_detect(path, "H1") ~ "H1",
      str_detect(path, "H3") ~ "H3",
      str_detect(path, "Yam") ~ "BYam",
      str_detect(path, "Vic") ~ "BVic",
    ),
  )

serology_all_tables_2021_fix_viruses %>%
  count(virus, subtype, path)

serology_all_tables_2021_fix_pids <- serology_all_tables_2021_fix_viruses %>%
  mutate(pid = recode(
    pid, "QCH-42-" = "QCH-042", "QCH-47-" = "QCH-047",
    "WCH-26" = "WCH-026", "WCH-26_" = "WCH-026", "WCH-26-" = "WCH-026",
    "WCH-28" = "WCH-028", "WCH-28_" = "WCH-028", "WCH-28-" = "WCH-028",
  ))

serology_all_tables <- bind_rows(
  serology_all_tables_2020_fix_pids, serology_all_tables_2021_fix_pids
) %>%
  mutate(virus_egg_cell = if_else(str_detect(virus, "e$"), "egg", "cell"))

serology_all_tables %>%
  count(virus, path)

# NOTE(sen) Should be missing
serology_all_tables %>% filter(pid == "WCH-025")

# NOTE(sen) Nothing should be missing
serology_all_tables %>% filter(!complete.cases(.))

# NOTE(sen) Shouldn't be any duplicates
serology_all_tables %>%
  group_by(pid, year, day, virus) %>%
  filter(n() > 1) %>%
  arrange(pid, year, day, virus)

write_csv(serology_all_tables %>% select(-path), "data/serology.csv")

#
# SECTION Participants
#

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

redcap_participants_request <- function(project_year) {
  redcap_request(
    project_year,
    "baseline_arm_1",
    "record_id,pid,date_screening,a1_gender,a2_dob,a3_atsi,email,mobile_number",
    exportDataAccessGroups = "true",
    rawOrLabel = "label"
  ) %>%
    mutate(across(c(redcap_data_access_group, a1_gender), tolower))
}

participants2020 <- redcap_participants_request(2020)
participants2021 <- redcap_participants_request(2021)
participants2022 <- redcap_participants_request(2022)

participants <- bind_rows(
  participants2020,
  participants2021 %>% filter(!pid %in% participants2020$pid),
  participants2022 %>% filter(!pid %in% participants2020$pid, !pid %in% participants2021$pid),
) %>%
  filter(!is.na(pid)) %>%
  # NOTE(sen) WCH-025 became WCH-818
  filter(pid != "WCH-025") %>%
  select(
    pid,
    site = redcap_data_access_group, gender = a1_gender, dob = a2_dob, atsi = a3_atsi,
    date_screening, email = email, mobile = mobile_number
  ) %>%
  mutate(
    recruitment_year = case_when(
      pid %in% participants2020$pid ~ 2020,
      pid %in% participants2021$pid ~ 2021,
      TRUE ~ 2022
    ),
    atsi = if_else(atsi == "Yes", 1, 0)
  )

# NOTE(sen) Some are missing baseline data
participants %>% filter(!complete.cases(.))

# NOTE(sen) Shouldn't be missing
participants %>%
  select(pid, site, recruitment_year, date_screening) %>%
  filter(!complete.cases(.))

extract_first_pid_digit <- function(string) {
  str_replace(string, ".*(\\d)\\d{2}.*", "\\1")
}

unique(extract_first_pid_digit(participants$pid))

participants_with_extras <- participants %>%
  mutate(
    age_screening = (date_screening - dob) / lubridate::dyears(1),
  )

fun_fix_pids <- function(pid) {
  str_replace(pid, "([[:alpha:]]{3})\\s?-?(\\d{3})", "\\1-\\2") %>%
    recode(
      "QCH 070" = "QCH-070",
      "JHH-824 (132)" = "JHH-824", # NOTE(sen) Changed within 2021
      "JHH-304 (820)" = "JHH-820", # NOTE(sen) Changed from 2021 to 2022
      "JHH- 826 (297)" = "JHH-297", # NOTE(sen) Changed from 2021 to 2022
      "JHH-334 (806)" = "JHH-806", # NOTE(sen) Changed from 2021 to 2022
    )
}

participants_fix_pid <- participants_with_extras %>%
  mutate(pid = fun_fix_pids(pid)) %>%
  group_by(pid) %>%
  filter(recruitment_year == min(recruitment_year)) %>%
  ungroup()

check_no_rows <- function(dat, msg) {
  if (nrow(dat) == 0) {
    message(crayon::green("no", msg))
  } else {
    message(crayon::red("found", msg))
    dat
  }
}

check_no_rows(
  participants_fix_pid %>% filter(str_length(pid) > 7),
  "participants with pids >7 characters"
)

check_no_rows(
  participants_fix_pid %>% group_by(pid) %>% filter(n() > 1),
  "participants with duplicate pids"
)

check_empty_set <- function(set, msg) {
  if (length(set) == 0) {
    message(crayon::green("no", msg))
  } else {
    message(crayon::red("found", msg))
  }
}

check_empty_set(
  setdiff(serology_all_tables$pid, participants_fix_pid$pid),
  "non-matching serology pids"
)

write_csv(participants_fix_pid, "data/participants.csv")

#
# SECTION Participant information that changes yearly
#

redcap_yearly_changes_request <- function(year) {
  redcap_request(year, "baseline_arm_1", "pid,record_id")
}

yearly_changes_raw <- redcap_yearly_changes_request(2020) %>%
  bind_rows(redcap_yearly_changes_request(2021)) %>%
  bind_rows(redcap_yearly_changes_request(2022)) %>%
  select(record_id, pid, redcap_project_year) %>%
  # NOTE(sen) WCH-025 became WCH-818
  filter(pid != "WCH-025")

yearly_changes_fix_pids <- yearly_changes_raw %>%
  filter(!is.na(pid)) %>%
  mutate(
    pid_og = pid,
    pid = fun_fix_pids(pid)
  )

# NOTE(sen) All PIDs should match
setdiff(yearly_changes_fix_pids$pid, participants_fix_pid$pid)
setdiff(participants_fix_pid$pid, yearly_changes_fix_pids$pid)

write_csv(yearly_changes_fix_pids, "data/yearly-changes.csv")

#
# SECTION Vaccination history
#

redcap_vaccination_history_request <- function(year) {
  redcap_request(
    year, "baseline_arm_1",
    "vac_2021,vac_2020,vac_2019,vac_2018,vac_2017,vac_2016,vac_2015,record_id",
    rawOrLabel = "label"
  )
}

vaccination_history_raw <- redcap_vaccination_history_request(2020) %>%
  bind_rows(redcap_vaccination_history_request(2021)) %>%
  bind_rows(redcap_vaccination_history_request(2022)) %>%
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(-record_id)

vaccination_history_no_duplicates <- vaccination_history_raw %>%
  pivot_longer(contains("vac_"), names_to = "year", values_to = "status") %>%
  group_by(pid, year) %>%
  summarise(.groups = "drop", status = unique(na.omit(status))) %>%
  mutate(
    year = str_replace(year, "vac_", "") %>% as.integer(),
    status = str_replace(status, "Yes - ", "")
  )

# NOTE(sen) Shouldn't be any missing data
vaccination_history_no_duplicates %>% filter(!complete.cases(.))

# NOTE(sen) Shouldn't be any duplicates
vaccination_history_no_duplicates %>%
  group_by(pid, year) %>%
  filter(n() > 1)

# NOTE(sen) There is also a vaccination instrument for the current year

redcap_vaccination_instrument_request <- function(year) {
  redcap_request(year, "vaccination_arm_1", "vaccinated,record_id")
}

vaccination_instrument_raw <- redcap_vaccination_instrument_request(2020) %>%
  bind_rows(redcap_vaccination_instrument_request(2021)) %>%
  bind_rows(redcap_vaccination_instrument_request(2022)) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance, -record_id) %>%
  rename(year = redcap_project_year)

# NOTE(sen) Shouldn't be any duplicates
vaccination_instrument_raw %>%
  group_by(pid, year) %>%
  filter(n() > 1) %>%
  arrange(pid, year)

# NOTE(sen) Shouldn't be any conflicting information
# TODO(sen) Contacted melbourne site about ALF-018 on 2021-08-26 through study email
vaccination_history_no_duplicates %>%
  inner_join(vaccination_instrument_raw, c("pid", "year")) %>%
  filter(
    (vaccinated != 1 & status == "Australia") |
      (vaccinated == 0 & status != "No")
  )

vaccination_history_with_instrument <- vaccination_history_no_duplicates %>%
  bind_rows(
    vaccination_instrument_raw %>%
      filter(
        !paste0(pid, year) %in%
          with(vaccination_history_no_duplicates, paste0(pid, year))
      ) %>%
      rename(status = vaccinated) %>%
      mutate(status = recode(status, "1" = "Australia", "0" = "No")) %>%
      filter(!is.na(status))
  )

# NOTE(sen) Shouldn't be any duplicates
vaccination_history_with_instrument %>%
  group_by(pid, year) %>%
  filter(n() > 1)

# NOTE(sen) Only 4 values allowed here
unique(vaccination_history_with_instrument$status)

# NOTE(sen) All ids should match
setdiff(vaccination_history_with_instrument$pid, participants_fix_pid$pid)

write_csv(vaccination_history_with_instrument, "data/vaccinations.csv")

#
# SECTION Covid vaccination
#

redcap_covax_request <- function(year) {
  redcap_request(
    year, "vaccination_arm_1",
    paste0(
      "record_id,",
      "covid_vac_brand,other_covax_brand,covid_vac_dose1_rec,covid_vacc_date1,covid_vac_batch1,",
      "covid_vac_brand2,other_covax_brand2,covid_vac_dose2_rec,covid_vacc_date2,covid_vac_batch2,",
      "covid_vac_brand3,other_covax_brand3,covid_vac_dose3_rec,covid_vacc_date3,covid_vac_batch3,",
      "covid_vac_brand4,other_covax_brand4,covid_vac_dose4_rec,covid_vacc_date4,covid_vac_batch4"
    )
  )
}

covax_request_raw <- redcap_covax_request(2021)

# TODO(sen) 2022?

covax_request <- covax_request_raw %>%
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance) %>%
  rename(covid_vac_brand1 = covid_vac_brand, other_covax_brand1 = other_covax_brand) %>%
  pivot_longer(
    c(-record_id, -redcap_project_year),
    names_pattern = "^(.*)(\\d).*$", names_to = c(".value", "dose")
  ) %>%
  rename(received = covid_vac_dose) %>%
  mutate(covid_vac_brand = recode(covid_vac_brand, "1" = "Pfizer", "2" = "Astra-Zeneca", "3" = "Other")) %>%
  filter(!is.na(received)) %>%
  mutate(brand = if_else(
    covid_vac_brand == "Other" & !is.na(other_covax_brand), other_covax_brand, covid_vac_brand
  )) %>%
  select(-covid_vac_brand, -other_covax_brand) %>%
  rename(date = covid_vacc_date, batch = covid_vac_batch) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  rename(year = redcap_project_year) %>%
  select(pid, year, dose, received, date, batch, brand)

write_csv(covax_request, "data/covid-vax.csv")

#
# SECTION Bleed dates
#

redcap_bleed_dates_request <- function(year) {
  redcap_request(
    year, "baseline_arm_1",
    "record_id,date_baseline_blood,date_7d_blood,date_14d_blood,date_end_season_blood"
  ) %>%
    mutate(across(
      c(date_baseline_blood, date_7d_blood, date_14d_blood, date_end_season_blood),
      as.character
    ))
}

bleed_dates_raw <- redcap_bleed_dates_request(2020) %>%
  bind_rows(redcap_bleed_dates_request(2021)) %>%
  bind_rows(redcap_bleed_dates_request(2022)) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance, -record_id) %>%
  rename(year = redcap_project_year)

bleed_dates_long <- bleed_dates_raw %>%
  pivot_longer(contains("date"), names_to = "timepoint", values_to = "date") %>%
  mutate(
    day = str_replace(timepoint, "date_", "") %>% str_replace("_blood", "") %>%
      recode("baseline" = "0", "7d" = "7", "14d" = "14", "end_season" = "220"),
    date = if_else(date == "NI", NA_character_, date)
  ) %>%
  select(-timepoint)

setdiff(bleed_dates_long$pid, participants_fix_pid$pid)

write_csv(bleed_dates_long, "data/bleed-dates.csv")

#
# SECTION Covid bleed dates
#

redcap_covid_bleed_dates_request <- function(year) {
  redcap_request(
    year, "baseline_arm_1",
    "record_id,covax_d0_sampdate,covax_d7_sampdate,covax_d14_sampdate"
  )
}

covid_bleed_dates_raw <- redcap_covid_bleed_dates_request(2020) %>%
  bind_rows(redcap_covid_bleed_dates_request(2021)) %>%
  bind_rows(redcap_covid_bleed_dates_request(2022)) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance, -record_id) %>%
  rename(year = redcap_project_year)

covid_bleed_dates_long <- covid_bleed_dates_raw %>%
  pivot_longer(contains("date"), names_to = "timepoint", values_to = "date") %>%
  mutate(
    day = str_replace(timepoint, "covax_d(\\d+)_sampdate", "\\1"),
  ) %>%
  select(-timepoint) %>%
  filter(!is.na(date))

write_csv(covid_bleed_dates_long, "data/covid-bleed-dates.csv")

#
# SECTION Consent
#

consent_vars <- c(
  "record_id",
  "consent",
  "add_bleed",
  "consent_future_use",
  "consent_covid",
  "study_group_vacc",
  "econsent_future_vacc",
  "consent_unvacc",
  "econsent_future_unvacc",
  "study_group_vacc_covax",
  "econsent_future_vacc_covax",
  "consent_date",
  "econsent_date_vacc",
  "econsent_date_unvacc",
  "consent_covid_date",
  "econsent_date_vacc_covax"
)

redcap_consent_request <- function(year) {
  redcap_request(
    year,
    "baseline_arm_1",
    paste0(consent_vars, collapse = ",")
  )
}

redcap_consent_raw <- redcap_consent_request(2020) %>%
  bind_rows(redcap_consent_request(2021)) %>%
  bind_rows(redcap_consent_request(2022))

redcap_consent_long <- redcap_consent_raw %>%
  mutate(
    consent_flu_manual = case_when(
      consent == 1 & add_bleed == 1 ~ "nested",
      consent == 1 ~ "main",
      consent == 0 ~ "no"
    ),
    consent_flu_electronic_vac = case_when(
      study_group_vacc == 1 ~ "main",
      study_group_vacc == 2 ~ "nested"
    ),
    consent_flu_electronic_unvac = case_when(
      consent_unvacc == 1 ~ "main",
      consent_unvacc == 0 ~ "no"
    ),
    consent_covid_manual = case_when(
      consent_covid == 1 ~ "main",
      consent_covid == 2 ~ "nested",
      consent_covid == 3 ~ "no"
    ),
    consent_covid_electronic = case_when(
      study_group_vacc_covax == 1 ~ "main",
      study_group_vacc_covax == 2 ~ "nested"
    )
  ) %>%
  select(
    record_id,
    redcap_project_year,
    consent_flu_manual,
    consent_flu_electronic_vac, consent_flu_electronic_unvac,
    consent_covid_manual, consent_covid_electronic
  ) %>%
  pivot_longer(-c(record_id, redcap_project_year), names_to = "form", values_to = "consent") %>%
  mutate(
    disease = str_replace(form, "^consent_([[:alpha:]]+)_.*$", "\\1"),
    form = str_replace(form, paste0("consent_", disease, "_"), "")
  ) %>%
  filter(!is.na(consent))

consent_dates <- redcap_consent_raw %>%
  select(record_id, redcap_project_year, contains("date")) %>%
  pivot_longer(contains("date"), names_to = "form", values_to = "date") %>%
  filter(!is.na(date)) %>%
  mutate(
    disease = if_else(
      form %in% c("consent_covid_date", "econsent_date_vacc_covax"),
      "covid",
      "flu"
    ),
    form = if_else(
      form %in% c("consent_date", "consent_covid_date"),
      "manual",
      "electronic"
    )
  )

redcap_consent_long_extra <- redcap_consent_long %>%
  left_join(consent_dates, c("record_id", "redcap_project_year", "form", "disease")) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(pid, year = redcap_project_year, date, disease, form, consent)

write_csv(redcap_consent_long_extra, "data/consent.csv")

covid_arms <- redcap_consent_long_extra %>%
  filter(disease == "covid", !is.na(consent), consent != "no") %>%
  group_by(pid) %>%
  summarise(covid_arm = paste(unique(na.omit(consent)), collapse = ","))

redcap_consent_use_long <- redcap_consent_raw %>%
  select(record_id, redcap_project_year, contains("___")) %>%
  pivot_longer(-c(record_id, redcap_project_year), names_to = "form", values_to = "consent_use") %>%
  mutate(
    disease = case_when(
      str_starts(form, "consent_future_use___") ~ "both",
      str_starts(form, "econsent_future_vacc___") ~ "flu",
      str_starts(form, "econsent_future_unvacc___") ~ "flu",
      str_starts(form, "econsent_future_vacc_covax___") ~ "covid",
    ),
    option = str_replace(form, ".*___(.*)$", "\\1") %>% recode("1" = "this", "2" = "other", "3" = "any"),
    form = case_when(
      str_starts(form, "consent_future_use___") ~ "manual",
      str_starts(form, "econsent_future_vacc___") ~ "electronic",
      str_starts(form, "econsent_future_unvacc___") ~ "electronic",
      str_starts(form, "econsent_future_vacc_covax___") ~ "electronic",
    ),
  )

redcap_consent_use_long_extra <- redcap_consent_use_long %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(pid, year = redcap_project_year, disease, form, option, consent_use)

write_csv(redcap_consent_use_long_extra, "data/consent-use.csv")

#
# SECTION Swabs
#

redcap_swabs_request <- function(year) {
  survey_events <- paste0("weekly_survey_", 1:52, "_arm_1", collapse = ",")
  all_events <- paste0("infection_arm_1,", survey_events)
  redcap_request(year, all_events, "record_id,swab_collection,samp_date,swab_result")
}

swabs <- redcap_swabs_request(2020) %>%
  bind_rows(redcap_swabs_request(2021)) %>%
  bind_rows(redcap_swabs_request(2022)) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance, -record_id) %>%
  rename(year = redcap_project_year)

swabs_no_missing <- swabs %>% filter(!is.na(swab_collection))

swabs_long <- swabs_no_missing %>%
  pivot_longer(contains("swab_result___"), names_to = "swab_virus", values_to = "swab_result") %>%
  mutate(swab_virus = str_replace(swab_virus, "swab_result___", "") %>% recode(
    "1" = "Flu A (unsubtyped)",
    "2" = "Flu A H3",
    "3" = "Flu A H1",
    "4" = "Flu B (no lineage)",
    "5" = "Flu B Vic",
    "6" = "Flu B Yam",
    "7" = "Flu C",
    "8" = "Parainfluenza",
    "9" = "Metapneumovirus",
    "10" = "Piconavirus",
    "11" = "Adenovirus",
    "12" = "Coronavirus OC43, 229E, NL63, HKU, SARS",
    "13" = "SARS-CoV-2",
    "14" = "Other",
    "15" = "Negative",
    "ni" = "NI",
  ))

swabs_long %>% count(swab_virus, swab_result) %>% print(n = 100)

write_csv(
  swabs_long %>%
    select(pid, year, samp_date, swab_collection, swab_virus, swab_result),
  "data/swabs.csv"
)

#
# SECTION Withdrawn
#

redcap_withdrawn_request <- function(year) {
  redcap_request(year, "withdrawal_arm_1", "record_id,withdrawn,withdrawal_date,withdrawal_reason,withdrawn_reentered")
}

withdrawn_raw <- redcap_withdrawn_request(2020) %>%
  bind_rows(redcap_withdrawn_request(2021)) %>%
  bind_rows(redcap_withdrawn_request(2022))

withdrawn <- withdrawn_raw %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance, -record_id) %>%
  select(pid, everything()) %>%
  mutate(
    withdrawn_reentered = replace_na(withdrawn_reentered, 0),
    #withdrawn_reentered = if_else(redcap_project_year == 2022, 0, 1),
  )

write_csv(withdrawn, "data/withdrawn.csv")

#
# SECTION Weekly surveys
#

redcap_weekly_survey_req <- function(year) {
  survey_events <- paste0("weekly_survey_", 1:52, "_arm_1", collapse = ",")
  weekly_survey_fields <- c(
    "record_id",
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
  redcap_request(year, survey_events, paste(weekly_survey_fields, collapse = ","))
}

weekly_surveys_raw <- redcap_weekly_survey_req(2020) %>%
  bind_rows(redcap_weekly_survey_req(2021)) %>%
  bind_rows(redcap_weekly_survey_req(2022))

weekly_surveys <- weekly_surveys_raw %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  mutate(survey_index = str_replace(redcap_event_name, "weekly_survey_(\\d+)_arm_1", "\\1")) %>%
  select(
    pid,
    year = redcap_project_year,
    survey_index,
    date = date_symptom_survey,
    ari = ari_definition,
    complete = weekly_symptom_survey_complete
  )

write_csv(weekly_surveys, "data/weekly-surveys.csv")
