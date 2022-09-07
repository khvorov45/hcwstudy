suppressPackageStartupMessages(library(tidyverse))

check_no_rows <- function(dat, msg) {
  if (nrow(dat) == 0) {
    message(crayon::green("OK: no", msg))
  } else {
    message(crayon::red("ERR: found", msg))
    dat
  }
}

check_empty_set <- function(set, msg) {
  if (length(set) == 0) {
    message(crayon::green("OK: no", msg))
  } else {
    message(crayon::red("ERR: found", msg))
  }
}

paste_unique_togethaaa <- function(vec) {
  result <- paste(unique(na.omit(vec)), collapse = ",")
  if_else(result == "", NA_character_, result)
}

#
# SECTION Serology
#

# system("data-raw/pull-NIHHCWserol.sh")

# NOTE(sen) Export tables from access, one csv per table
# system("data-raw/export-NIHHCWserol.sh")

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

check_virus_fix <- function(serology_data) {
  summ <- serology_data %>%
    group_by(path) %>%
    summarise(unique_viruses = length(unique(virus)), unique_subtypes = length(unique(subtype)))
  check_no_rows(summ %>% filter(unique_viruses != 1), "not 1 unique virus per serology file")
  check_no_rows(summ %>% filter(unique_subtypes != 1), "not 1 unique subtype per serology file")
}

check_virus_fix(serology_all_tables_2020_fix_viruses)

# NOTE(sen) WCH-025 became WCH-818 and we have V0 WCH-818 data
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

check_virus_fix(serology_all_tables_2021_fix_viruses)

serology_all_tables_2021_fix_pids <- serology_all_tables_2021_fix_viruses %>%
  mutate(pid = recode(
    pid,
    "QCH-42-" = "QCH-042", "QCH-47-" = "QCH-047",
    "WCH-26" = "WCH-026", "WCH-26_" = "WCH-026", "WCH-26-" = "WCH-026",
    "WCH-28" = "WCH-028", "WCH-28_" = "WCH-028", "WCH-28-" = "WCH-028",
    "ALF-092" = "ALF-819",
  ))

serology_all_tables <- bind_rows(
  serology_all_tables_2020_fix_pids, serology_all_tables_2021_fix_pids
) %>%
  mutate(virus_egg_cell = if_else(str_detect(virus, "e$"), "egg", "cell"))

check_virus_fix(serology_all_tables)
check_no_rows(serology_all_tables %>% filter(pid == "WCH-025"), "WCH-025")
check_no_rows(serology_all_tables %>% filter(pid == "ALF-092"), "ALF-092")
check_no_rows(serology_all_tables %>% filter(!complete.cases(.)), "serology anything missing")

check_no_rows(
  serology_all_tables %>%
    group_by(pid, year, day, virus) %>%
    filter(n() > 1) %>%
    arrange(pid, year, day, virus),
  "serology duplicates"
)

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

fun_fix_pids <- function(pid) {
  str_replace(pid, "([[:alpha:]]{3})\\s?-?(\\d{3})", "\\1-\\2") %>%
    recode(
      "QCH 070" = "QCH-070",
      "JHH-824 (132)" = "JHH-824", # NOTE(sen) Changed within 2021
      "JHH-304 (820)" = "JHH-820", # NOTE(sen) Changed from 2021 to 2022
      "JHH- 826 (297)" = "JHH-297", # NOTE(sen) Changed from 2021 to 2022
      "JHH-334 (806)" = "JHH-806", # NOTE(sen) Changed from 2021 to 2022
      "JHH-830 (082)" = "JHH-082", # NOTE(sen) Changed from 2021 to 2022
      "WCH-025" = "WCH-818", # NOTE(sen) Changed study group in 2020
      "ALF-092" = "ALF-819", # NOTE(sen) Changed study group in 2022
    )
}

participants <- bind_rows(participants2020, participants2021, participants2022) %>%
  filter(!is.na(pid)) %>%
  select(
    pid,
    site = redcap_data_access_group, gender = a1_gender, dob = a2_dob, atsi = a3_atsi,
    date_screening, email = email, mobile = mobile_number, redcap_project_year,
  ) %>%
  mutate(
    pid = fun_fix_pids(pid),
    atsi = if_else(atsi == "Yes", 1, 0)
  )

check_no_rows(
  participants %>% filter(is.na(pid)),
  "participants with missing pids"
)

check_no_rows(
  participants %>% filter(!str_detect(pid, "^(PCH|CHW|WCH|JHH|QCH|ALF)-\\d{3}$")),
  "participants with non-conforming pids"
)

check_no_rows(
  participants %>% count(pid) %>% filter(n > 3, pid != "WCH-818"),
  "(unkndown) pids present more than 3 times"
)

# TODO(sen) Check conflicting info
participants_with_extras <- participants %>%
  group_by(pid, site) %>%
  summarise(
    .groups = "drop",
    gender = last(na.omit(gender)),
    dob = last(na.omit(dob)),
    atsi = last(na.omit(atsi)),
    date_screening = first(na.omit(date_screening)),
    email = last(na.omit(email)),
    mobile = last(na.omit(mobile)),
    recruitment_year = first(na.omit(redcap_project_year)),
  ) %>%
  mutate(
    age_screening = (date_screening - dob) / lubridate::dyears(1),
  )

check_empty_set(
  setdiff(serology_all_tables$pid, participants_with_extras$pid),
  "non-matching serology pids"
)

check_no_rows(
  participants_with_extras %>% group_by(pid) %>% filter(n() > 1),
  "duplicate pids"
)

check_no_rows(
  participants_with_extras %>%
    select(pid, site, recruitment_year, date_screening) %>%
    filter(!complete.cases(.)),
  "participants non-baseline missing"
)

write_csv(participants_with_extras, "data/participants.csv")

#
# SECTION Participant information that changes yearly
#

redcap_yearly_changes_request <- function(year) {
  redcap_request(year, "baseline_arm_1", "pid,record_id")
}

yearly_changes_raw <- redcap_yearly_changes_request(2020) %>%
  bind_rows(redcap_yearly_changes_request(2021)) %>%
  bind_rows(redcap_yearly_changes_request(2022)) %>%
  select(record_id, pid, redcap_project_year)

yearly_changes_fix_pids <- yearly_changes_raw %>%
  filter(!is.na(pid)) %>%
  mutate(
    pid_og = pid,
    pid = fun_fix_pids(pid)
  )

check_empty_set(
  setdiff(yearly_changes_fix_pids$pid, participants_with_extras$pid),
  "yearly changes not in pariticpants"
)

check_empty_set(
  setdiff(participants_with_extras$pid, yearly_changes_fix_pids$pid),
  "participants not in yearly changes"
)

check_no_rows(
  yearly_changes_fix_pids %>%
    group_by(pid_og, redcap_project_year) %>%
    filter(n() > 1),
  "PID (OG) used more than once within a year"
)

check_no_rows(
  yearly_changes_fix_pids %>%
    group_by(pid, redcap_project_year) %>%
    filter(n() > 1, !pid %in% c("WCH-818", "ALF-819")),
  "(unknown) PID (fixed) used more than once within a year"
)

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
  filter(!is.na(status)) %>%
  mutate(
    status = str_replace(status, "Yes - ", "")
  ) %>%
  group_by(pid, year) %>%
  filter(n() == 1 | status != "Unknown") %>%
  summarise(
    .groups = "drop",
    status = paste(unique(status), collapse = ","),
  ) %>%
  mutate(
    year = str_replace(year, "vac_", "") %>% as.integer(),
  )

check_no_rows(
  vaccination_history_no_duplicates %>% filter(!complete.cases(.)),
  "missing vaccination history"
)

check_no_rows(
  vaccination_history_no_duplicates %>%
    group_by(pid, year) %>%
    filter(n() > 1),
  "vaccination history duplicates (screening)"
)

check_no_rows(
  vaccination_history_no_duplicates %>%
    filter(str_detect(status, ",")),
  "vaccination history conflicts (screening)"
)

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
  rename(year = redcap_project_year) %>%
  filter(!is.na(vaccinated))

vaccination_instrument_raw_no_duplicates <- vaccination_instrument_raw %>%
  group_by(pid, year) %>%
  summarise(.groups = "drop", vaccinated = as.integer(any(vaccinated == 1)))

check_no_rows(
  vaccination_instrument_raw_no_duplicates %>%
    group_by(pid, year) %>%
    filter(n() > 1) %>%
    arrange(pid, year),
  "vaccination instrument duplicates"
)

# NOTE(sen) Believe the vaccination instrument more
vaccination_instrument_renamed <- vaccination_instrument_raw_no_duplicates %>%
  rename(status = vaccinated) %>%
  mutate(status = recode(status, "1" = "Australia", "0" = "No")) %>%
  filter(!is.na(status))

vaccination_history_with_instrument <- vaccination_instrument_renamed %>%
  bind_rows(
    vaccination_history_no_duplicates %>%
      filter(!paste0(pid, year) %in% with(vaccination_instrument_renamed, paste0(pid, year)))
  )

check_no_rows(
  vaccination_history_with_instrument %>%
    group_by(pid, year) %>%
    filter(n() > 1),
  "combined vaccination history duplicates"
)

check_empty_set(
  setdiff(vaccination_history_with_instrument$pid, participants_with_extras$pid),
  "vaccination pids not in participant pids"
)

check_no_rows(
  vaccination_history_with_instrument %>%
    group_by(pid, year) %>%
    filter(n() > 1),
  "vaccination history (final) duplicates"
)

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

covax_request_raw <- redcap_covax_request(2021) %>% bind_rows(redcap_covax_request(2022))

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
  select(pid, redcap_project_year, dose, received, date, batch, brand)

check_no_rows(
  covax_request %>% filter(lubridate::year(date) <= 2020),
  "wrong covax date"
)

check_no_rows(
  covax_request %>%
    group_by(pid, dose) %>%
    filter(
      length(unique(na.omit(received))) > 1 | length(unique(na.omit(date))) > 1 |
      length(unique(na.omit(batch))) > 1 | length(unique(na.omit(brand))) > 1
    ) %>%
    arrange(pid, dose),
  "conflicting covid vaccination info"
)

check_no_rows(
  covax_request %>% filter(received == 0) %>% filter(!is.na(date) | !is.na(batch) | !is.na(brand)),
  "covid vax not received but data recorded"
)

covax_dedup <- covax_request %>%
  group_by(pid, dose) %>%
  summarise(
    .groups = "drop",
    received = first(unique(na.omit(received))),
    date = first(unique(na.omit(date))),
    batch = first(unique(na.omit(batch))),
    brand = first(unique(na.omit(brand))),
  ) %>%
  filter(received == 1) %>%
  select(-received)

check_no_rows(
  covax_dedup %>% group_by(pid, dose) %>% filter(n() > 1),
  "duplicate covid vax"
)

write_csv(covax_dedup, "data/covid-vax.csv")

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
  select(pid, year, day, date) %>%
  filter(!is.na(date))

bleed_dates_long_no_duplicates <- bleed_dates_long %>%
  group_by(pid, year, day) %>%
  summarise(.groups = "drop", date = max(lubridate::ymd(date)))

check_empty_set(
  setdiff(bleed_dates_long_no_duplicates$pid, participants_with_extras$pid),
  "bleed dates pids not in participants pids"
)

check_no_rows(
  bleed_dates_long_no_duplicates %>%
    group_by(pid, year, day) %>%
    filter(n() > 1),
  "bleed dates duplicates"
)

write_csv(bleed_dates_long_no_duplicates, "data/bleed-dates.csv")

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
  select(pid, year, day, date) %>%
  filter(!is.na(date))

check_no_rows(
  covid_bleed_dates_long %>%
    group_by(pid, year, day) %>%
    filter(n() > 1),
  "covid bleed duplicates"
)

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
    form = case_when(
      form %in% c("consent_date", "consent_covid_date") ~ "manual",
      form %in% c("econsent_date_vacc") ~ "electronic_vac",
      form %in% c("econsent_date_unvacc") ~ "electronic_unvac",
      form %in% c("econsent_date_vacc_covax") ~ "electronic",
      TRUE ~ NA_character_
    )
  )

check_no_rows(consent_dates %>% filter(is.na(form)), "missing consent form id on dates")

redcap_consent_long_extra <- redcap_consent_long %>%
  left_join(consent_dates, c("record_id", "redcap_project_year", "form", "disease")) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(pid, year = redcap_project_year, date, disease, form, consent)

redcap_consent_long_extra_no_duplicates <- redcap_consent_long_extra %>%
  group_by(pid, year, disease, form) %>%
  filter(
    any(is.na(date)) | date == max(date),
    !(pid == "ALF-819" & consent == "main" & year == 2022 & form == "electronic_vac")
) %>%
  ungroup()

check_no_rows(
  redcap_consent_long_extra_no_duplicates %>%
    group_by(pid, year, disease, form) %>%
    filter(n() > 1),
  "consent duplicates"
)

write_csv(redcap_consent_long_extra_no_duplicates, "data/consent.csv")

covid_arms <- redcap_consent_long_extra_no_duplicates %>%
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
      str_starts(form, "econsent_future_vacc___") ~ "electronic_vac",
      str_starts(form, "econsent_future_unvacc___") ~ "electronic_unvac",
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

redcap_consent_use_long_extra_no_duplicates <- redcap_consent_use_long_extra %>%
  group_by(pid, year, disease, form, option) %>%
  summarise(.groups = "drop", consent_use = as.integer(any(consent_use == 1)))

check_no_rows(
  redcap_consent_use_long_extra_no_duplicates %>%
    group_by(pid, year, disease, form, option) %>%
    filter(n() > 1) %>%
    arrange(pid, year, disease, form, option),
  "duplicates in consent use"
)

write_csv(redcap_consent_use_long_extra_no_duplicates, "data/consent-use.csv")

#
# SECTION Post-infection swabs and bleeds
#

postinf_vars <- c(
  "record_id",
  "swab_collection",
  "samp_date",
  "swab_result",
  "d7_postinfection_blood",
  "d14_postinfection_blood",
  "d30_postinfection_blood"
)

redcap_postinf_request <- function(year) {
  survey_events <- paste0("weekly_survey_", 1:52, "_arm_1", collapse = ",")
  all_events <- paste0("infection_arm_1,", survey_events)
  redcap_request(year, all_events, paste(postinf_vars, collapse = ","))
}

redcap_postinf <- redcap_postinf_request(2020) %>%
  bind_rows(redcap_postinf_request(2021)) %>%
  bind_rows(redcap_postinf_request(2022)) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance, -record_id) %>%
  rename(year = redcap_project_year) %>%
  mutate(postinf_instance = row_number())

swabs_no_missing <- redcap_postinf %>%
  filter(!is.na(swab_collection), swab_collection == 1) %>%
  select(-contains("postinfection_blood"), -swab_collection)

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
  )) %>%
  select(pid, year, postinf_instance, samp_date, swab_virus, swab_result)

check_no_rows(
  swabs_long %>%
    group_by(pid, year, postinf_instance, swab_virus) %>%
    filter(n() > 1),
  "duplicate swabs"
)

write_csv(swabs_long, "data/swabs.csv")

postinf_bleed_dates <- redcap_postinf %>%
  select(-contains("swab_result___")) %>%
  pivot_longer(contains("postinfection_blood"), names_to = "day", values_to = "bleed_date") %>%
  mutate(day = str_replace(day, "d(\\d+)_postinfection_blood", "\\1") %>% as.integer()) %>%
  filter(!is.na(bleed_date)) %>%
  select(pid, year, postinf_instance, samp_date, day, swab_collection, bleed_date)

check_no_rows(
  postinf_bleed_dates %>%
    group_by(pid, year, postinf_instance, day) %>%
    filter(n() > 1),
  "duplicate post-infection bleeds"
)

write_csv(postinf_bleed_dates, "data/postinf-bleed-dates.csv")

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
  filter(!is.na(withdrawn), withdrawn == 1) %>%
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance, -record_id) %>%
  select(pid, everything()) %>%
  mutate(
    withdrawn_reentered = replace_na(withdrawn_reentered, 0),
    # withdrawn_reentered = if_else(redcap_project_year == 2022, 0, 1),
  )

check_no_rows(
  withdrawn %>%
    group_by(pid, redcap_project_year) %>%
    filter(n() > 1),
  "withdrawal duplicates"
)

write_csv(withdrawn, "data/withdrawn.csv")

#
# SECTION Weekly surveys
#

redcap_weekly_survey_req <- function(year) {
  survey_events <- paste0("weekly_survey_", 1:52, "_arm_1", collapse = ",")
  # NOTE(sen) The more you include here the longer it will take
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
    "symptom_duration",
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
  redcap_request(year, survey_events, paste(weekly_survey_fields, collapse = ",")) %>%
    mutate(symptom_duration = as.integer(symptom_duration))
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
    symptom_duration = symptom_duration,
    complete = weekly_symptom_survey_complete
  )

weekly_surveys_no_duplicates <- weekly_surveys %>%
  group_by(pid, year, survey_index) %>%
  filter(n() == 1 | complete == 2) %>%
  ungroup()

check_no_rows(
  weekly_surveys_no_duplicates %>%
    group_by(pid, year, survey_index) %>%
    filter(n() > 1) %>%
    arrange(pid, year, survey_index),
  "weekly surveys duplicates"
)

write_csv(weekly_surveys_no_duplicates, "data/weekly-surveys.csv")

#
# SECTION Daily surveys
#

redcap_daily_survey_req <- function(year) {
  survey_events <- paste0(paste0("daily_survey_", 1:6, "_"), "w", 1:52, "_arm_1", collapse = ",")
  # NOTE(sen) The more you include here the longer it will take
  daily_survey_fields <- c(
    "record_id",
    "date_symptom_diary",
    "symp_present"
  )
  redcap_request(year, survey_events, paste(daily_survey_fields, collapse = ","))
}

daily_surveys_raw <- redcap_daily_survey_req(2020) %>%
  bind_rows(redcap_daily_survey_req(2021)) %>%
  bind_rows(redcap_daily_survey_req(2022))

daily_surveys <- daily_surveys_raw %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  mutate(
    daily_survey_index = str_replace(redcap_event_name, "daily_survey_(\\d+)_w(\\d+)_arm_1", "\\1"),
    weekly_survey_index = str_replace(redcap_event_name, "daily_survey_(\\d+)_w(\\d+)_arm_1", "\\2")
  ) %>%
  select(
    pid,
    year = redcap_project_year,
    daily_survey_index,
    weekly_survey_index,
    date = date_symptom_diary,
    symp_present,
  )

write_csv(daily_surveys, "data/daily-surveys.csv")
