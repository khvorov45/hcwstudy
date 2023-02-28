suppressPackageStartupMessages(library(tidyverse))

# Check consent signatures
# Check consent names
# Check bleed dates go in ascending order
# Check dob/other baseline stuff makes sense
# Check serology d7/d14 is only present for those with a vaccination record

all_csv_files <- tools::list_files_with_exts("data-problems", "csv")
walk(all_csv_files, file.remove)

save_split <- function(data, name) {
	stopifnot(!str_detect(name, "-"))
	write_csv(data, paste0("data-problems/", name, ".csv"))
	data %>%
		group_by(site) %>%
		group_walk(~write_csv(.x, paste0("data-problems/", name, "-", .y, ".csv")))
}

participants <- read_csv("data/participants.csv", col_types = cols())
withdrawn <- read_csv("data/withdrawn.csv", col_types = cols()) %>% left_join(participants %>% select(pid, site), "pid")
consent <- read_csv("data/consent.csv", col_types = cols()) %>% left_join(participants %>% select(pid, site), "pid")
serology <- read_csv("data/serology.csv", col_types = cols()) %>% left_join(participants %>% select(pid, site), "pid")
serology_covid <- read_csv("data/serology-covid.csv", col_types = cols()) %>% left_join(participants %>% select(pid, site), "pid")
bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols()) %>% left_join(participants %>% select(pid, site), "pid")
swabs <- read_csv("data/swabs.csv", col_types = cols()) %>% left_join(participants %>% select(pid, site), "pid")

stopifnot(sum(is.na(withdrawn$site)) == 0)
stopifnot(sum(is.na(consent$site)) == 0)
stopifnot(sum(is.na(serology$site)) == 0)
stopifnot(sum(is.na(serology_covid$site)) == 0)
stopifnot(sum(is.na(bleed_dates$site)) == 0)
stopifnot(sum(is.na(swabs$site)) == 0)

participants %>%
	select(-email, -mobile) %>%
	filter(!complete.cases(.), !pid %in% withdrawn$pid) %>%
	save_split("missing_baseline")

consent %>%
  group_by(pid, year, disease) %>%
  filter(length(unique(na.omit(consent))) > 1) %>%
  ungroup() %>%
  mutate(form = paste(disease, form, sep = "_")) %>%
  arrange(form) %>%
  select(-disease, -date) %>%
  pivot_wider(names_from = "form", values_from = "consent") %>%
  arrange(pid, site, year) %>%
  save_split("consent_conflicts")

serology %>%
	left_join(
		consent %>%
			filter(consent != "no", disease == "flu") %>%
			select(pid, year) %>%
			distinct() %>%
			mutate(consented = TRUE),
		c("pid", "year")
	) %>%
	filter(is.na(consented)) %>%
	select(pid, site, year) %>%
	distinct() %>%
	save_split("bleed_no_consent")

serology_covid %>%
	filter(vax_inf == "V") %>%
	left_join(
		consent %>%
			filter(consent != "no", disease == "covid") %>%
			select(pid) %>%
			distinct() %>%
			mutate(consented = TRUE),
		c("pid")
	) %>%
	filter(is.na(consented)) %>%
	select(pid, site) %>%
	distinct() %>%
	save_split("bleed_no_consent_covid")

# NOTE(sen) All serology pids should match
# setdiff(serology$pid, bleed_dates$pid)

missing_bleed_dates <- serology %>%
	inner_join(bleed_dates, c("pid", "year", "day", "site")) %>%
	filter(is.na(date))

save_split(missing_bleed_dates, "missing_bleed_dates")

vaccinations <- read_csv("data/vaccinations.csv", col_types = cols()) %>%
	inner_join(participants, "pid")

vaccinations_before_recruitment <- vaccinations %>%
	filter(year < recruitment_year)

vaccinations_after_recruitment <- vaccinations %>%
	filter(year >= recruitment_year)

missing_vaccination_history <- participants %>%
	filter(!pid %in% vaccinations_before_recruitment$pid, !pid %in% withdrawn$pid)

save_split(missing_vaccination_history, "missing_vaccination_history")

missing_vaccination_records <- participants %>%
	filter(!pid %in% vaccinations_after_recruitment$pid, !pid %in% withdrawn$pid)

save_split(missing_vaccination_records, "missing_vaccination_records")


swabs_missing_date <- swabs %>%
	filter(is.na(samp_date), !pid %in% withdrawn$pid) %>%
	group_by(pid, site, year, postinf_instance, samp_date) %>%
	summarise(.groups = "drop", viruses = paste(swab_virus[swab_result == 1], collapse = ","))

save_split(swabs_missing_date, "swabs_missing_date")

withdrawn_missing_date <- withdrawn %>%
	filter(is.na(withdrawal_date)) %>%
	arrange(pid)

save_split(withdrawn_missing_date, "withdrawn_missing_date")

#
# SECTION Covid bleed dates but no covid vaccination records
#

covid_bleed_dates <- read_csv("data/covid-bleed-dates.csv", col_types = cols())

covid_vax <- read_csv("data/covid-vax.csv", col_types = cols()) %>% left_join(participants %>% select(pid, site), "pid")
covid_vax_only_rec <- covid_vax %>% select(pid) %>% distinct() %>% mutate(received = TRUE)

covid_bleeds_no_vax <- covid_bleed_dates %>%
	left_join(covid_vax_only_rec, "pid") %>%
	filter(is.na(received)) %>%
	inner_join(participants %>% select(pid, site), "pid")

save_split(covid_bleeds_no_vax, "covid_bleeds_no_vax")

#
# SECTION Missing covid vax data
#

covid_vax %>%
	group_by(pid, site) %>%
	filter(!all(1:3 %in% dose)) %>%
	select(pid, site, dose) %>%
	summarise(doses = paste0(dose, collapse = " "), .groups = "drop") %>%
	bind_rows(
		participants %>% 
			filter(!pid %in% covid_vax$pid) %>%
			select(pid, site) %>%
			mutate(doses = "")
	) %>%
	filter(!pid %in% withdrawn$pid) %>%
	filter(pid %in% (consent %>% filter(disease == "covid", consent != "no") %>% pull(pid) %>% unique())) %>%
	arrange(pid) %>%
	save_split("missing_covax_dose")

#
# SECTION Covid vax dates
#

covid_vax %>%
	group_by(pid) %>%
	arrange(dose) %>%
	mutate(date_prev = lag(date), interval = date - date_prev) %>%
	filter(interval < 0) %>%
	save_split("covid_vax_dates_not_ascending")

covid_vax %>%
	filter(is.na(date)) %>%
	save_split("covid_vax_missing_dates")

covid_vax %>%
	filter(is.na(brand)) %>%
	save_split("covid_vax_missing_brand")

#
# SECTION D14 serology and vaccination records
#

serology %>% 
	filter(day == 14) %>% 
	select(pid, year) %>% 
	inner_join(participants %>% select(pid, site), "pid") %>%
	distinct() %>% 
	left_join(vaccinations %>% select(pid, year, status), c("pid", "year")) %>% 
	filter(is.na(status)) %>%
	save_split("bled_d14_no_vax_record")
