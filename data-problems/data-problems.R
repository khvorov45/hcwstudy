suppressPackageStartupMessages(library(tidyverse))

# Double-check consent + bleed
# Check consent signatures
# Check consent names
# Check bleed dates and vaccination dates go in ascending order

save_split <- function(data, name) {
	write_csv(data, paste0("data-problems/", name, ".csv"))
	data %>%
		group_by(site) %>%
		group_walk(~write_csv(.x, paste0("data-problems/", name, "-", .y, ".csv")))
}

participants <- read_csv("data/participants.csv", col_types = cols())

withdrawn <- read_csv("data/withdrawn.csv", col_types = cols())

missing_baseline <- participants %>%
	filter(!complete.cases(.), !pid %in% withdrawn$pid)

save_split(missing_baseline, "missing_baseline")

consent <- read_csv("data/consent.csv", col_types = cols()) %>%
	inner_join(participants %>% select(pid, site), "pid")

consent_conflicts <- consent %>%
  group_by(pid, year, disease) %>%
  filter(length(unique(na.omit(consent))) > 1) %>%
  ungroup() %>%
  mutate(form = paste(disease, form, sep = "_")) %>%
  arrange(form) %>%
  select(-disease, -date) %>%
  pivot_wider(names_from = "form", values_from = "consent") %>%
  arrange(pid, site, year)

save_split(consent_conflicts, "consent_conflicts")

consent_multiple <- consent %>%
  group_by(pid, year, disease) %>%
  filter(length(na.omit(consent)) > 1) %>%
  ungroup() %>%
  pivot_wider(names_from = "form", values_from = "consent") %>%
  arrange(pid, site, year, disease)

save_split(consent_multiple, "consent_multiple")

bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols()) %>%
	inner_join(participants %>% select(pid, site), "pid")

serology <- read_csv("data/serology.csv", col_types = cols())

# NOTE(sen) All serology pids should match
# setdiff(serology$pid, bleed_dates$pid)

missing_bleed_dates <- serology %>%
	inner_join(bleed_dates, c("pid", "year", "day")) %>%
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

swabs <- read_csv("data/swabs.csv", col_types = cols()) %>%
	inner_join(participants %>% select(pid, site), "pid")

swabs_missing_date <- swabs %>%
	filter(is.na(samp_date), !pid %in% withdrawn$pid) %>%
	group_by(pid, site, year, postinf_instance, samp_date) %>%
	summarise(.groups = "drop", viruses = paste(swab_virus[swab_result == 1], collapse = ","))

save_split(swabs_missing_date, "swabs_missing_date")

withdrawn_missing_date <- withdrawn %>%
	inner_join(participants %>% select(pid, site), "pid") %>%
	filter(is.na(withdrawal_date)) %>%
	arrange(pid)

save_split(withdrawn_missing_date, "withdrawn_missing_date")

#
# SECTION Covid bleed dates but no covid vaccination records
#

covid_bleed_dates <- read_csv("data/covid-bleed-dates.csv", col_types = cols())

covid_vax <- read_csv("data/covid-vax.csv", col_types = cols())

covid_vax_only_rec <- covid_vax %>%
	group_by(pid, year) %>%
	summarise(.groups = "drop", rec_any = any(received == 1))

covid_bleeds_no_vax <- covid_bleed_dates %>%
	left_join(covid_vax_only_rec, c("pid", "year")) %>%
	filter(is.na(rec_any) | !rec_any) %>%
	inner_join(participants %>% select(pid, site), "pid")

save_split(covid_bleeds_no_vax, "covid_bleeds_no_vax")
