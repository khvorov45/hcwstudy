suppressPackageStartupMessages(library(tidyverse))

check_no_rows <- function(dat, msg) {
  if (nrow(dat) == 0) {
    message(crayon::green("OK: no", msg))
  } else {
    message(crayon::red("ERR: found", msg))
    print(dat)
  }
}

check_empty_set <- function(set, msg) {
  if (length(set) == 0) {
    message(crayon::green("OK: no", msg))
  } else {
    message(crayon::red("ERR: found", msg))
  }
}

quick_summary <- function(data) {
	walk(colnames(data), function(colname) {
		cat(crayon::red(paste0(colname, ": ")))
		unique_vals <- unique(data[[colname]])
		unique_vals_short <- unique_vals
		end_str <- ""
		if (length(unique_vals) > 10) {
			unique_vals_short <- unique_vals[1:10]
			end_str <- crayon::red(" ...")
		}
		unique_vals_short <- sort(unique_vals_short, na.last = FALSE)
		cat(paste(map_chr(unique_vals_short, function(val) {
			if (is.na(val)) {
				crayon::magenta(val)
			} else if (is.character(val)) {
				paste0("'", crayon::cyan(val), "'")
			} else if (is.numeric(val)) {
				crayon::green(val)
			} else if (is.logical(val)) {
				crayon::yellow(val)
			} else {
				crayon::red(val)
			}
		}), collapse = " "))
		cat(end_str, "\n", sep = "")
	})
}

#
# SECTION Serology
#

# NOTE(sen) Export tables from access, one csv per table
# file.remove(list.files("data-raw/serology", full.names = TRUE))
# file.remove(list.files("data-raw/serology-covid", full.names = TRUE))
# system("data-raw/pull-NIHHCWserol.sh")
# system("data-raw/export-NIHHCWserol.sh")

serology_all_tables <- list.files("data-raw/serology", pattern = "HI_.*_202[012]_", full.names = TRUE) %>%
  c(
    "data-raw/serology/Y3_2023_H1_egg_all.xlsx",
    "data-raw/serology/BVic_egg_2022_all.xlsx",
    "data-raw/serology/New Caledonia HI_H1N1 immunogenicity_allplates.xlsx",
    "data-raw/serology/NIH_H1N1_ABrazil_11_egg_All.xlsx"
  ) %>%
  map_dfr(function(path) {

    tbl <- NULL
    if (str_ends(path, "xlsx")) {
      if (str_ends(path, "Y3_2023_H1_egg_all.xlsx")) {
        tbl <- readxl::read_excel(path, "2023-01-23_HCW_Y3_H1N1_egg", guess_max = 1e5) %>%
          mutate(Designation = "A/Victoria/2570/2019e", year = 2022)
      } else if (str_ends(path, "BVic_egg_2022_all.xlsx")) {
        tbl <- readxl::read_excel(path, guess_max = 1e5) %>%
          mutate(Designation = "B/Austria/1359417/2021e", year = 2022) %>%
          group_by(PID, VisitN) %>%
          filter(n() == 1 | `Plate Name` == "B Vic Repeat Plate 1") %>%
          ungroup()
      } else if (str_ends(path, "New Caledonia HI_H1N1 immunogenicity_allplates.xlsx")) {
        tbl <- readxl::read_excel(path, guess_max = 1e5) %>%
          mutate(Designation = "A/NewCaledonia/20/1999e", year = as.integer(StudyYear))
      } else if (str_ends(path, "NIH_H1N1_ABrazil_11_egg_All.xlsx")) {
        tbl <- readxl::read_excel(path, guess_max = 1e5, na = c("", "repeat", "No result")) %>%
          mutate(Designation = "A/Brazil/11/1978e", year = as.integer(Year))
      }
    } else {
      year <- 2022
      if (str_starts(basename(path), "HI")) {
        year <- str_replace(path, ".*HI_.*_(202[012])_.*", "\\1") %>% as.integer()
      }
      if (str_ends(path, "sera.csv")) {
        year <- str_replace(path, ".*_(202[012])sera\\.csv$", "\\1") %>% as.integer()
      }
      tbl <- read_csv(path, col_types = cols(), guess_max = 1e5) %>%
        mutate(year = year)
    }

    if (str_ends(path, "HI_H3N2egg_2022_V73.csv")) {
      tbl <- mutate(tbl, Designation = "A/Darwin/9/2021e")
    }

    tblcols <- colnames(tbl)
    virus_col_name <- tblcols[str_detect(tblcols, "Designation")]
    stopifnot(length(virus_col_name) == 1)
    time_col_name <- tblcols[tblcols %in% c("Time", "TimeN", "Timepoint", "timeN", "Visit", "VisitN")][[1]]
    stopifnot(length(time_col_name) == 1)

    if (is.character(tbl$Titer)) {
      tbl <- mutate(tbl, Titer = str_replace(Titer, "<", "") %>% str_trim() %>% as.integer())
    }

    tbl %>%
      mutate(
        vax_inf = str_trunc(!!rlang::sym(time_col_name), 1, ellipsis = "") %>% toupper() %>% replace_na("V"),
        vax_inf = if_else(vax_inf %in% c("V", "I"), vax_inf, "V"),
        day = str_replace(!!rlang::sym(time_col_name), "^[VvIi]", "") %>% as.integer(),
        path = path
      ) %>%
      select(
        pid = PID, year, day, virus = !!rlang::sym(virus_col_name),
        titre = Titer, path, vax_inf
      ) %>%
      # NOTE(sen) The whole point is to have a titre measurement, no titre -
      # don't insert into the table at all
      filter(!is.na(titre))
  })

check_no_rows(serology_all_tables %>% filter(pid != "JHH-018", is.na(day)), "serology missing day")

serology_all_tables_fix_day <- serology_all_tables %>%
  # NOTE(sen) Only bled at baseline in 2020
  mutate(day = if_else(pid == "JHH-018", 0L, day))

# NOTE(sen) Fix virus names
serology_all_tables_fix_viruses <- serology_all_tables_fix_day %>%
  mutate(
    virus = virus %>%
      # NOTE(sen) Inconsistent left-pad
      str_replace("/(\\d)/", "/0\\1/") %>%
      # NOTE(sen) Let's assume that "c" at the end is not important because it's
      # not in the virus table
      str_replace("c$", "") %>%
      str_replace("\\s*egg$", "e") %>%
      str_replace("\\s*e$", "e") %>%
      str_replace("A_Victoria_2570_2019", "A/Victoria/2570/2019") %>%
      str_replace("SouthAustralia|SouthAust", "South Australia") %>%
      str_replace("SthAust_34_19Cell", "A/South Australia/34/2019") %>%
      str_replace("\\s*\\(?IVR-?190\\)?\\s*", "e") %>%
      str_replace("\\s*IVR Egg\\s*", "e") %>%
      str_replace("\\s*IVR_208\\s*", "e") %>%
      str_replace("\\s*IVR\\s*", "e") %>%
      str_replace("\\s*[Ee]gg split$", "e") %>%
      str_replace("\\s*[Cc]ell split$", "") %>%
      str_replace("\\s*Cell$", "") %>%
      str_replace("\\s*Volume looks right$", "") %>%
      str_trim(),

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
    ),
    virus_egg_cell = if_else(str_detect(virus, "e$"), "egg", "cell"),

    virus_clade = recode(
      virus,
      # NOTE(sen) From VCM trees (data-raw/trees)
      "A/Brisbane/02/2018" = "6B.1A.1",
      "A/Brisbane/02/2018e" = "6B.1A.1",
      "A/Darwin/09/2021e" = "3C.2a1b.2a.2",
      "A/Darwin/726/2019" = "3C.2a1b.1b",
      "A/Hong Kong/2671/2019e" = "3C.2a1b",
      "A/South Australia/34/2019" = "2a1b",
      "A/South Australia/34/2019e" = "2a1b",
      "A/Victoria/2570/2019" = "6B.1A.5a.2",
      "A/Victoria/2570/2019e" = "6B.1A.5a.2",
      "B/Austria/1359417/2021e" = "V1A.3a.2",
      "B/Phuket/3073/2013" = "Y3",
      "B/Phuket/3073/2013e" = "Y3",
      "B/Washington/02/2019" = "V1A.3a",
      "B/Washington/02/2019e" = "V1A.3a",
      "A/NewCaledonia/20/1999e" = "None",
      "A/Brazil/11/1978e" = "None",
    ),

    virus_vaccine =
      year == 2020 & (str_starts(virus, "A/Brisbane/02/2018") | str_starts(virus, "A/South Australia/34/2019") | str_starts(virus, "B/Washington/02/2019") | str_starts(virus, "B/Phuket/3073/2013")) |
      year == 2021 & (str_starts(virus, "A/Victoria/2570/2019") | str_starts(virus, "A/Hong Kong/2671/2019") | str_starts(virus, "A/Darwin/726/2019") | str_starts(virus, "B/Washington/02/2019") | str_starts(virus, "B/Phuket/3073/2013")) |
      year == 2022 & (str_starts(virus, "A/Victoria/2570/2019") | str_starts(virus, "A/Darwin/09/2021") | str_starts(virus, "B/Austria/1359417/2021") | str_starts(virus, "B/Phuket/3073/2013"))
  )

check_no_rows(
  serology_all_tables_fix_viruses %>%
    filter(virus_vaccine) %>%
    group_by(year, subtype, virus_egg_cell) %>%
    filter(length(unique(virus)) != 1),
  "wrong vaccine viruses"
)

check_no_rows(
  serology_all_tables_fix_viruses %>% select(virus, virus_clade) %>% distinct() %>% filter(virus == virus_clade),
  "missing clade"
)

check_no_rows(
  serology_all_tables_fix_viruses %>% filter(is.na(subtype)),
  "missing subtype"
)

check_virus_fix <- function(serology_data) {
  summ <- serology_data %>%
    group_by(path) %>%
    summarise(
      unique_viruses = length(unique(virus)),
      unique_subtypes = length(unique(subtype)),
      viruses = paste0(unique(virus), collapse = "; ")
    )
  check_no_rows(summ %>% filter(unique_viruses != 1) %>% select(viruses), "not 1 unique virus per serology file")
  check_no_rows(summ %>% filter(unique_subtypes != 1), "not 1 unique subtype per serology file")
}

check_virus_fix(serology_all_tables_fix_viruses)
check_empty_set(sort(unique(serology_all_tables_fix_viruses$virus)) %>% .[!str_detect(., "\\d{4}e?")], "virus names dont end on 4 digits and optional e")

fun_fix_pids <- function(pid) {
  str_replace(pid, "([[:alpha:]]{3})\\s?-?(\\d{3})", "\\1-\\2") %>%
    recode(
      "QCH-42-" = "QCH-042", "QCH-47-" = "QCH-047",
      "WCH-26" = "WCH-026", "WCH-26_" = "WCH-026", "WCH-26-" = "WCH-026",
      "WCH-28" = "WCH-028", "WCH-28_" = "WCH-028", "WCH-28-" = "WCH-028",
      "QCH 070" = "QCH-070",
      "JHH-824 (132)" = "JHH-824", # NOTE(sen) Changed within 2021
      "JHH-304 (820)" = "JHH-820", # NOTE(sen) Changed from 2021 to 2022
      "JHH-304" = "JHH-820",
      "JHH-826 (297)" = "JHH-297", # NOTE(sen) Changed from 2021 to 2022
      "JHH-826" = "JHH-297",
      "JHH-334 (806)" = "JHH-806", # NOTE(sen) Changed from 2021 to 2022
      "JHH-830 (082)" = "JHH-082", # NOTE(sen) Changed from 2021 to 2022
      "JHH-830" = "JHH-082",
      "JHH-305 (813)" = "JHH-813", # NOTE(sen) Changed from 2021 to 2022
      "WCH-025" = "WCH-818", # NOTE(sen) Changed study group in 2020
      "ALF-092" = "ALF-819", # NOTE(sen) Changed study group in 2022
    )
}

serology_all_tables_fix_pids <- serology_all_tables_fix_viruses %>%
  # NOTE(sen) WCH-025 became WCH-818 and we have V0 WCH-818 data
  filter(pid != "WCH-025") %>%
  mutate(pid = fun_fix_pids(pid))

check_no_rows(serology_all_tables_fix_pids %>% filter(pid == "WCH-025"), "WCH-025")
check_no_rows(serology_all_tables_fix_pids %>% filter(pid == "ALF-092"), "ALF-092")
check_no_rows(serology_all_tables_fix_pids %>% filter(!complete.cases(.)), "serology anything missing")

check_no_rows(
  serology_all_tables_fix_pids %>%
    group_by(pid, year, day, virus, vax_inf) %>%
    filter(n() > 1) %>%
    arrange(pid, year, day, virus, vax_inf),
  "serology duplicates"
)

write_csv(serology_all_tables_fix_pids %>% select(-path), "data/serology.csv")

sercovid_raw <- read_csv("data-raw/serology-covid/sVNT_higher_sensitivity.csv", col_types = cols())

check_no_rows(
  sercovid_raw %>%
    filter(SampleComment != "repeat") %>%
    group_by(Sample_ID, Strain, Batch) %>%
    filter(n() > 1) %>%
    arrange(Sample_ID, Strain),
  "raw covid serology duplicates"
)

sercovid <- sercovid_raw %>%
  mutate(
    sample_id_split = str_split(Sample_ID, "[-_]"),
    pid = paste(map(sample_id_split, 1), map(sample_id_split, 2), sep = "-") %>% str_trim() %>% fun_fix_pids(),
    timepoint_id = as.character(map(sample_id_split, 3)),
    bleed_year = as.character(map(sample_id_split, 4)),
    bleed_year = if_else(bleed_year == "NULL", NA_character_, bleed_year),
    bleed_year = as.integer(bleed_year) %>% replace_na(2021),
    bleed_day_id = timepoint_id %>% str_replace("^[^\\d]+", "") %>% as.integer(),
    bleed_day_id = if_else(bleed_day_id == 220 & bleed_year == 2020, 0L, bleed_day_id),
    vax_inf = if_else(str_starts(timepoint_id, "I"), "I", "V"),
    bleed_flu_covid = if_else(str_starts(timepoint_id, "C") | str_starts(timepoint_id, "I"), "C", "F"),
    strain = Strain,
  ) %>%
  group_by(pid, bleed_flu_covid, bleed_day_id, bleed_year, vax_inf, strain) %>%
  filter(Batch == max(Batch) | all(is.na(Batch))) %>%
  filter(n() == 1 | TP == "pre_r") %>%
  ungroup() %>%
  arrange(pid) %>%
  select(pid, ic50 = IC50, vax_inf, bleed_flu_covid, bleed_year, bleed_day_id, strain) %>%
  filter(!is.na(ic50))

# NOTE(sen) Quick sanity check, most covid pids should be in flu serology as well
check_no_rows(
  sercovid %>% filter(pid != "WCH-076", !pid %in% serology_all_tables_fix_pids$pid),
  "covid serology bad PIDs"
)

check_no_rows(
  sercovid %>% group_by(pid, vax_inf, bleed_year, bleed_day_id, bleed_flu_covid, strain) %>% filter(n() > 1) %>% arrange(pid, bleed_day_id, vax_inf, bleed_flu_covid, bleed_year),
  "covid serology duplicates"
)

check_no_rows(
  sercovid %>% filter(!complete.cases(.)),
  "serology covid missing data"
)

write_csv(sercovid, "data/serology-covid.csv")

seradeno_raw <- read_csv("data-raw/serology-covid/Ad5_hexon_ELISA.csv", col_types = cols())

seradeno <- seradeno_raw %>%
  mutate(
    pid = str_replace(Sample_ID, "^([[:alpha:]]{3}-\\d{3}).*$", "\\1"),
    timepoint_id = str_replace(Sample_ID, "^[[:alpha:]]{3}-\\d{3}\\s*-", ""),
    bleed_flu_covid = str_sub(timepoint_id, 1, 1) %>% recode("V" = "F"),
    bleed_year = str_replace(timepoint_id, "^.*(\\d{4})$", "\\1") %>% as.integer(),
  ) %>%
  select(pid, bleed_flu_covid, bleed_year, bleed_day_id = Visit, ic50 = Ad5AbEC50) %>%
  filter(!is.na(ic50))

check_no_rows(
  seradeno %>% group_by(pid, bleed_flu_covid, bleed_day_id, bleed_year) %>% filter(n() > 1),
  "adeno serology duplicates"
)

write_csv(seradeno, "data/serology-adenovirus.csv")

serlandscape_raw <- read_csv("data-raw/serology-landscapes/HI.csv", col_types = cols(), guess_max = 1e5)

check_no_rows(
  serlandscape_raw %>%
    group_by(Sample_ID, VirusN) %>%
    filter(n() > 1) %>%
    arrange(Sample_ID, VirusN),
  "raw landscape serology duplicates"
)

serlandscape <- serlandscape_raw %>%
  mutate(
    sample_id_split = str_split(Sample_ID, "[-_]"),
    pid = paste(map(sample_id_split, 1), map(sample_id_split, 2), sep = "-"),
    year = as.integer(map(sample_id_split, 3)),
    day = as.character(map(sample_id_split, 4)) %>% str_replace("^[vV]", "") %>% as.integer(),
  ) %>%
  select(pid, year, day, virus_number = VirusN, titre = Titer)

check_no_rows(
  serlandscape %>% filter(!pid %in% serology_all_tables_fix_pids$pid),
  "landscape serology bad PIDs"
)

check_no_rows(
  serlandscape %>%
    group_by(pid, year, day, virus_number) %>%
    filter(n() > 1),
  "landscape serology duplicates"
)

landscape_viruses_raw <- read_csv("data-raw/serology-landscapes/Viruses.csv", col_types = cols())

landscape_viruses <- landscape_viruses_raw %>%
  mutate(virus_egg_cell = recode(Egg_Cell, "0" = "Egg", "1" = "Cell")) %>%
  select(virus_number = VirusN, virus = Virus_Name, virus_egg_cell, virus_year = Virus_Year)

serlandscape_with_viruses <- serlandscape %>%
  # NOTE(sen) Remove duplicates
  group_by(pid, year, day, virus_number) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  left_join(landscape_viruses, "virus_number")

check_no_rows(serlandscape_with_viruses %>% filter(is.na(virus)), "landscape missing virus")

write_csv(serlandscape_with_viruses, "data/serology-landscapes.csv")

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
  names <- c(
    "record_id",
    "pid",
    "date_screening",
    "a1_gender",
    "a2_dob",
    "a3_atsi",
    "a5_height",
    "a6_weight",
    "c1_yrs_employed",
    "d1_future_vacc",
    "email",
    "mobile_number"
  )
  redcap_request(
    project_year,
    "baseline_arm_1",
    paste0(names, collapse = ","),
    exportDataAccessGroups = "true",
    rawOrLabel = "label"
  ) %>%
    mutate(across(c(redcap_data_access_group, a1_gender), tolower))
}

participants2020 <- redcap_participants_request(2020)
participants2021 <- redcap_participants_request(2021)
participants2022 <- redcap_participants_request(2022)

participants <- bind_rows(participants2020, participants2021, participants2022) %>%
  filter(!is.na(pid)) %>%
  select(
    pid,
    site = redcap_data_access_group, gender = a1_gender, dob = a2_dob, atsi = a3_atsi,
    height = a5_height, weight = a6_weight, years_employed = c1_yrs_employed, future_vacc_intent = d1_future_vacc,
    date_screening, email = email, mobile = mobile_number, redcap_project_year,
  ) %>%
  mutate(
    pid = fun_fix_pids(pid),
    atsi = if_else(atsi == "Yes", 1, 0),
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
    atsi = last(na.omit(atsi)),
    height = last(na.omit(height)),
    weight = last(na.omit(weight)),
    years_employed = first(na.omit(years_employed)),
    date_screening = first(na.omit(date_screening)),
    future_vacc_intent = first(na.omit(future_vacc_intent)),
    email = last(na.omit(email)),
    mobile = last(na.omit(mobile)),
    recruitment_year = first(na.omit(redcap_project_year)),
  ) %>%
  mutate(
    age_screening = (date_screening - dob) / lubridate::dyears(1),
    bmi =  weight / (height / 100)^2
  )

check_empty_set(
  setdiff(serology_all_tables_fix_pids$pid, participants_with_extras$pid),
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
  redcap_request(year, "baseline_arm_1", "pid,record_id,a4_children,children1,c2_emp_status,emp_status1,c3_occupation,occupation1")
}

yearly_changes_raw <- redcap_yearly_changes_request(2020) %>%
  bind_rows(redcap_yearly_changes_request(2021)) %>%
  bind_rows(redcap_yearly_changes_request(2022)) %>%
  select(
    record_id, pid, redcap_project_year,
    children_base = a4_children, children_ret = children1,
    emp_status_base = c2_emp_status, emp_status_ret = emp_status1,
    occupation_base = c3_occupation, occupation_ret = occupation1,
  )

yearly_changes_baseret_merged <- yearly_changes_raw %>%
  arrange(pid) %>%
  mutate(
    children = if_else(!is.na(children_ret), children_ret, children_base),
    emp_status = if_else(!is.na(emp_status_ret), emp_status_ret, emp_status_base),
    occupation = if_else(!is.na(occupation_ret), occupation_ret, occupation_base),
    emp_status = recode(emp_status,
      "1" = "Full time",
      "2" = "Casual",
      "3" = "Part time",
    ),
    occupation = recode(occupation,
      "1" = "Medical",
      "2" = "Nursing",
      "3" = "Allied health",
      "4" = "Laboratory",
      "5" = "Administrative",
      "6" = "Ancillary",
      "8" = "Research",
      "7" = "Other",
    )
  ) %>%
  select(-contains("ret"), -contains("base"))

yearly_changes_fix_pids <- yearly_changes_baseret_merged %>%
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
# SECTION Work department
#

redcap_workdept_request <- function(project_year) {
  names <- c(
    "record_id",
    "c4_workdept",
    "workdept1"
  )
  redcap_request(
    project_year,
    "baseline_arm_1",
    paste0(names, collapse = ",")
  )
}

workdept_raw <- redcap_workdept_request(2020) %>%
  bind_rows(redcap_workdept_request(2021)) %>%
  bind_rows(redcap_workdept_request(2022))

# TODO(sen) Update with survey from returning participants
workdept <- workdept_raw %>%
  select(record_id, redcap_project_year, contains("c4")) %>%
  pivot_longer(contains("c4"), names_to = "dept", values_to = "status") %>%
  mutate(dept = str_replace(dept, "c4_workdept___", "") %>% recode(
    "1" = "Emergency Department",
    "2" = "Critical Care or Intensive Care Unit",
    "3" = "General Medicine and/or Medical Specialities",
    "4" = "Paediatrics and/or Paediatric Specialities",
    "5" = "Surgery and/or Surgical Specialties",
    "6" = "Gynaecology and/or Obstetrics",
    "7" = "Oncology and/or Haematology",
    "8" = "Radiology",
    "9" = "Outpatient clinic",
    "10" = "Pharmacy",
    "11" = "Laboratory",
    "12" = "Nutrition",
    "13" = "Social Work",
    "14" = "Physiotherapy",
    "15" = "Occupational therapy",
    "16" = "Other",
  )) %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  filter(.by = pid, redcap_project_year == min(redcap_project_year)) %>%
  select(pid, dept, status) %>%
  distinct()

check_no_rows(filter(workdept, .by = c(pid, dept), n() > 1), "workdept duplicates")

write_csv(workdept, "data/workdept.csv")

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

vaccination_history_processed <- vaccination_history_raw %>%
  pivot_longer(contains("vac_"), names_to = "year", values_to = "status") %>%
  filter(!is.na(status)) %>%
  mutate(
    status = str_replace(status, "Yes - ", ""),
    year = str_replace(year, "vac_", "") %>% as.integer(),
  ) %>%
  group_by(pid, year) %>%
  filter(n() == 1 | status != "Unknown") %>%
  ungroup()

vaccination_history_no_duplicates <- vaccination_history_processed %>%
  group_by(pid, year) %>%
  filter(redcap_project_year == max(redcap_project_year)) %>%
  filter(row_number() == 1) %>%
  ungroup()

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
  vaccination_history_processed %>%
    group_by(pid, year) %>%
    filter(length(unique(status)) > 1) %>%
    arrange(pid, year),
  "vaccination history conflicts (screening)"
)

# NOTE(sen) There is also a vaccination instrument for the current year

redcap_vaccination_instrument_request <- function(year) {
  redcap_request(year, "vaccination_arm_1", "vaccinated,record_id,vaccination_date,vac_brand,vac_batch")
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
  rename(year = redcap_project_year, batch = vac_batch) %>%
  filter(!is.na(vaccinated)) %>%
  mutate(brand = recode(vac_brand, "1" = "GSK", "2" = "Sanofi", "3" = "Seqirus"))

vaccination_instrument_raw_no_duplicates <- vaccination_instrument_raw %>%
  group_by(pid, year, brand, batch, vaccination_date) %>%
  summarise(.groups = "drop", vaccinated = as.integer(any(vaccinated == 1))) %>%
  filter(!(pid == "WCH-818" & is.na(brand)))

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
      select(-redcap_project_year) %>%
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

# TODO(sen) 5th dose info
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
  covax_request %>%
    filter(lubridate::year(date) <= 2020) %>%
    # NOTE(sen) Seems to be fine
    filter(!(pid == "JHH-383" & dose == 1)),
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

covax_fix_brand <- covax_dedup %>%
  mutate(brand = case_when(
    str_detect(tolower(brand), "moderna") ~ "Moderna",
    str_detect(tolower(brand), "novavax") ~ "Novavax trial",
    TRUE ~ brand
  ) %>% recode(
    "Covid vax study- Novovax" = "Novavax trial",
    "SARS-CoV-2 rS Nanoparticle vaccine and Matrix-M1 Adjuvant" = "Novavax trial",
    "Novatrial- study vaccine" = "Novavax trial",
    "Sanofi Booster Vaccine Trial" = "Sanofi trial",
  ))

write_csv(covax_fix_brand, "data/covid-vax.csv")

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

# NOTE(sen) QCH-081 and QCH-181 d14 2020 bleed dates are verified.

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

redcap_consent_special_cases <- redcap_consent_long_extra_no_duplicates %>%
  mutate(consent = case_when(
    pid == "JHH-031" & year == 2020 & disease == "flu" & form == "electronic_vac" ~ "main",
    pid == "JHH-801" & year == 2020 & disease == "flu" & form == "electronic_vac" ~ "nested",
    pid == "JHH-805" & year == 2020 & disease == "flu" & form == "electronic_vac" ~ "nested",
    pid == "JHH-807" & year == 2020 & disease == "flu" & form == "electronic_vac" ~ "nested",
    pid == "JHH-810" & year == 2020 & disease == "flu" & form == "electronic_vac" ~ "nested",
    TRUE ~ consent,
  ))

write_csv(redcap_consent_special_cases, "data/consent.csv")

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
  "d30_postinfection_blood",
  "ari_swab_notes"
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
  select(-redcap_repeat_instrument, -redcap_repeat_instance, -record_id) %>%
  rename(year = redcap_project_year) %>%
  mutate(postinf_instance = row_number())

swabs_no_missing <- redcap_postinf %>%
  filter(!is.na(swab_collection), swab_collection == 1) %>%
  select(-contains("postinfection_blood"), -swab_collection) %>%
  # NOTE(sen) Try to get rid of "partially collected" swabs sydney has apparenlty recorded
  filter(!(str_starts(pid, "CHW") & year <= 2022 & is.na(samp_date)))

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

postinf_comments <- redcap_postinf %>%
  mutate(survey_week = if_else(
    redcap_event_name == "infection_arm_1",
    NA_character_,
    str_replace(redcap_event_name, "^weekly_survey_(\\d+)_arm_1$", "\\1")
  )) %>%
  filter(!is.na(ari_swab_notes)) %>%
  select(pid, year, survey_week, postinf_instance, ari_swab_notes)

write_csv(postinf_comments, "data/postinf-comments.csv")

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
    # "pt_contact",
    # "nonpt_contact",
    # "absence",
    # "duration_absent",
    # "health_advice",
    # "medical_service_v2", # NOTE(sen) Checkbox
    "diagnosis_v2",
    # "new_illness",
    # "new_illness_desc",
    "week_surv_gen_com",
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
  mutate(
    survey_index = str_replace(redcap_event_name, "weekly_survey_(\\d+)_arm_1", "\\1"),
    diagnosis = recode(diagnosis_v2, "1" = "flu", "2" = "covid", "3" = "other", "4" = "unknown"),
  ) %>%
  select(
    pid,
    year = redcap_project_year,
    survey_index,
    date = date_symptom_survey,
    respiratory,
    systemic,
    fever,
    chills,
    headache,
    myalgia,
    malaise,
    n_systemic,
    cough,
    sorethroat,
    runnynose,
    chestpain,
    breathing,
    n_respiratory,
    ari = ari_definition,
    symptom_duration = symptom_duration,
    diagnosis,
    comments = week_surv_gen_com,
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

#
# SECTION Medial history (comorbidities)
#

# TODO(sen) Update with survey for returning participants
redcap_medhx_req <- function(year) {
  fields <- c(
    "record_id",
    "b1_medicalhx"
  )
  redcap_request(year, "baseline_arm_1", paste(fields, collapse = ","))
}

medhx_raw_2020 <- redcap_medhx_req(2020)
medhx_raw_2021 <- redcap_medhx_req(2021)
medhx_raw_2022 <- redcap_medhx_req(2022)

comorbidities <- bind_rows(medhx_raw_2020, medhx_raw_2021, medhx_raw_2022) %>%
  pivot_longer(starts_with("b1_medicalhx"), names_to = "condition", values_to = "status") %>%
  filter(status == 1) %>%
  select(record_id, redcap_project_year, condition) %>%
  mutate(
    condition = str_replace(condition, "^b1_medicalhx___", "") %>%
      recode(
        "1" = "Cardiac disease",
        "2" = "Renal disease",
        "3" = "Chronic respiratory condition",
        "4" = "Haematological disorder",
        "10" = "Chronic neurological condition",
        "5" = "Pregnancy",
        "6" = "Immunocompromising condition",
        "7" = "Diabetes or other metabolic disorder",
        "8" = "Smoker",
        "9" = "None",
      )
  ) %>%
  filter(condition != "None") %>%
  inner_join(
    yearly_changes_fix_pids %>%
      select(record_id, pid, redcap_project_year),
    c("record_id", "redcap_project_year")
  ) %>%
  select(pid, condition) %>%
  distinct() %>%
  arrange(pid, condition)

write_csv(comorbidities, "data/comorbidities.csv")
