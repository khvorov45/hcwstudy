library(tidyverse)

# SECTION Serology

system("data-raw/pull-NIHHCWserol.sh")

# NOTE(sen) Export tables from access, one csv per table
system("data-raw/export-NIHHCWserol.sh")

viruses <- read_csv("data-raw/NIH Viruses.csv", col_types = cols()) %>%
  select(name = Virus_Name, short_name = Short_Name, clade = Clade) %>%
  mutate(egg = str_ends(name, "e") | str_ends(name, "\\(IVR-190\\)"))

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

# NOTE(sen) Let's assume JHH-018's titres are all at V0 (they are missing day)
serology_all_tables_fix_day <- serology_all_tables_2020 %>% mutate(
  day = if_else(pid == "JHH-018", 0L, day)
)

# NOTE(sen) We shouldn't have any missing data because missing titres were
# removed above
serology_all_tables_fix_day %>% filter(!complete.cases(.))

# NOTE(sen) Fix virus names
serology_all_tables_fix_viruses <- serology_all_tables_fix_day %>%
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
    # NOTE(sen) Let's assume south australia 2019 is actually a 'p' as per the
    # virus table
    virus = recode(
      virus,
      "A/South Australia/34/2019" = "A/South Australia/34/2019p"
    ),
  )

# NOTE(sen) Viruses should match
serology_viruses_fixed <- unique(serology_all_tables_fix_viruses$virus)
setdiff(serology_viruses_fixed, viruses$name)

serology_all_tables_fix_pids <- serology_all_tables_fix_viruses %>%
  # NOTE(sen) WCH-025 became WCH-818 and we seem to have V0 WCH-818 data
  filter(pid != "WCH-025")

# NOTE(sen) Shouldn't be any duplicates
serology_all_tables_fix_pids %>%
  group_by(pid, year, day, virus) %>%
  filter(n() > 1) %>%
  arrange(pid, year, day, virus)

write_csv(viruses %>% rename_with(~ paste0("virus_", .x)), "data/viruses.csv")
write_csv(serology_all_tables_fix_pids %>% select(-path), "data/serology.csv")

# SECTION Participants

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
    read_csv(col_types = cols()) %>%
    mutate(redcap_project_year = project_year)
}

redcap_participants_request <- function(project_year) {
  redcap_request(
    project_year,
    "baseline_arm_1",
    "record_id,pid,date_screening,a1_gender,a2_dob",
    exportDataAccessGroups = "true",
    rawOrLabel = "label"
  ) %>%
    mutate(across(c(redcap_data_access_group, a1_gender), tolower))
}

participants2020 <- redcap_participants_request(2020)
participants2021 <- redcap_participants_request(2021)

participants <- bind_rows(
  participants2020 %>% filter(!pid %in% participants2021$pid),
  participants2021
) %>%
  filter(!is.na(pid)) %>%
  # NOTE(sen) WCH-025 became WCH-818
  filter(pid != "WCH-025") %>%
  select(
    pid,
    site = redcap_data_access_group, gender = a1_gender, dob = a2_dob,
    date_screening
  ) %>%
  mutate(
    recruitment_year = if_else(pid %in% participants2020$pid, 2020, 2021)
  )

# NOTE(sen) Some are missing baseline data
participants %>% filter(!complete.cases(.))
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
    # TODO(sen) This seems to be the most reliable way of doing it right now
    # but consent forms would be better if they didn't conflict
    arm = if_else(extract_first_pid_digit(pid) == "8", "nested", "main")
  )

fun_fix_pids <- function(pid) {
  str_replace(pid, "([[:alpha:]]{3})(\\d{3})", "\\1-\\2") %>%
    recode("QCH 070" = "QCH-070")
}

participants_fix_pid <- participants_with_extras %>%
  mutate(pid = fun_fix_pids(pid))

# NOTE(sen) Shouldn't be any duplicates
participants_fix_pid %>%
  group_by(pid) %>%
  filter(n() > 1)

# NOTE(sen) All serology pids should match
setdiff(serology_all_tables_fix_pids$pid, participants_fix_pid$pid)

write_csv(participants_fix_pid, "data/participants.csv")

# SECTION Participant information that changes yearly

redcap_yearly_changes_request <- function(year) {
  redcap_request(year, "baseline_arm_1", "pid,record_id")
}

yearly_changes_raw <- redcap_yearly_changes_request(2020) %>%
  bind_rows(redcap_yearly_changes_request(2021)) %>%
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

# SECTION Vaccination history

redcap_vaccination_history_request <- function(year) {
  redcap_request(
    year, "baseline_arm_1",
    "vac_2020,vac_2019,vac_2018,vac_2017,vac_2016,vac_2015,record_id",
    rawOrLabel = "label"
  )
}

vaccination_history_raw <- redcap_vaccination_history_request(2020) %>%
  bind_rows(redcap_vaccination_history_request(2021)) %>%
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

# NOTE(sen) Should see one change due to the above
all.equal(vaccination_instrument_raw_fixed, vaccination_instrument_raw)

# NOTE(sen) Shouldn't be any conflicting information
vaccination_history_no_duplicates %>%
  inner_join(vaccination_instrument_raw_fixed, c("pid", "year")) %>%
  filter(
    (vaccinated != 1 & status == "Australia") |
      (vaccinated == 0 & status != "No")
  )

vaccination_history_with_instrument <- vaccination_history_no_duplicates %>%
  bind_rows(
    vaccination_instrument_raw_fixed %>%
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

# SECTION Bleed dates

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

# SECTION Consent

# TODO(sen) Pull consent and consent conflicts

# SECTION Swabs

redcap_swabs_request <- function(year) {
  survey_events <- paste0("weekly_survey_", 1:52, "_arm_1", collapse = ",")
  all_events <- paste0("infection_arm_1,", survey_events)
  redcap_request(year, all_events, "record_id,swab_collection")
}

swabs <- redcap_swabs_request(2020) %>%
  bind_rows(redcap_swabs_request(2021)) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance, -record_id) %>%
  rename(year = redcap_project_year)

swabs_no_missing <- swabs %>% filter(!is.na(swab_collection))

write_csv(swabs_no_missing, "data/swabs.csv")
