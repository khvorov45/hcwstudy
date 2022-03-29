library(tidyverse)

swabs <- read_csv("data/swabs.csv")

swabs %>%
	filter(swab_collection == 1 & swab_result == 1 & swab_virus == "SARS-CoV-2")

serology <- read_csv("data/serology.csv")
serology2021 <- serology %>% filter(year == 2021)

serology2021 %>%
	group_by(pid) %>%
	summarise(measure_count = n(), timepoint_count = length(unique(day))) %>%
	summarise(
		pid_count = n(),
		measure_count = sum(measure_count),
		min_t = min(timepoint_count),
		max_t = max(timepoint_count),
	)

vaccinations <- read_csv("data/vaccinations.csv")

vac_hist <- vaccinations %>%
	filter(year <= 2020, year >= 2016) %>%
	group_by(pid) %>%
	summarise(prior_vacs = sum(status == "Australia" | status == "Overseas"))

vac_hist %>%
	count()

serology2021_extra <- serology2021 %>%
	left_join(vac_hist, "pid")

serology2021_extra %>%
	group_by(prior_vacs) %>%
	summarise(pid_count = length(unique(pid))) #%>%
	# filter(prior_vacs != 0) %>%
	# summarise(sum(pid_count))

summarise_prop <- function(vec) {
	vec <- na.omit(vec)
	prop <- sum(vec) / length(vec)
	ci <- PropCIs::exactci(sum(vec), length(vec), 0.95)
	low <- ci$conf.int[[1]]
	high <- ci$conf.int[[2]]
	f <- \(x) round(x * 100)
	prop_low_high <- glue::glue("{f(prop)} ({f(low)}, {f(high)})")
	tibble(prop, low, high, prop_low_high)
}

serology2021_extra %>%
	filter(day == 0 | day == 14) %>%
	pivot_wider(names_from = "day", values_from = "titre") %>%
	mutate(ratio = `14` / `0`, seroconv = ratio >= 4) %>%
	filter(!is.na(ratio)) %>%
	mutate(prior_vacs = as.character(prior_vacs)) %>%
	bind_rows(mutate(., prior_vacs = "total")) %>%
	group_by(prior_vacs, subtype, virus_egg_cell) %>%
	summarise(summarise_prop(seroconv), .groups = "drop") %>%
	mutate(
		vs = paste(subtype, virus_egg_cell) %>% str_replace_all(" ", "_"),
	) %>%
	select(prior_vacs, vs, prop_low_high) %>%
	pivot_wider(names_from = "vs", values_from = prop_low_high) %>%
	write_csv("data-summary/misc-seroconv.csv")

participants <- read_csv("data/participants.csv")
withdrawn <- read_csv("data/withdrawn.csv")

participants2021 <- participants %>%
	filter(!pid %in% (withdrawn %>% filter(redcap_project_year == 2020) %>% pull(pid)))

participants2021 %>%
	mutate(withdrawn2021 = pid %in% (withdrawn %>% filter(redcap_project_year == 2021) %>% pull(pid))) %>%
	filter(recruitment_year == 2020) %>%
	count(withdrawn2021)

participants2021 %>%
	count()

swabs %>%
	left_join(participants %>% select(pid, site), "pid") %>%
	filter(swab_collection == 1) %>%
	group_by(site) %>%
	summarise(n = length(unique(paste0(pid, samp_date))))

consent <- read_csv("data/consent.csv")

consent %>%
	filter(disease == "covid", !is.na(consent), consent != "no") %>%
	left_join(participants %>% select(pid, site)) %>%
	#group_by(site) %>%
	summarise(n = length(unique(pid)))

participants %>% filter(recruitment_year == 2021)
participants %>% filter(recruitment_year == 2020)

participants2021 %>% filter(recruitment_year == 2020)

participants2021 %>%
	left_join(vac_hist, "pid") %>%
	#left_join(vaccinations %>% filter(year == 2021)) %>%
	filter(
		prior_vacs != 5,
		!pid %in% (withdrawn %>% filter(redcap_project_year == 2021) %>% pull(pid)),
		arm == "nested",
		#status == "No",
	) %>%
	count(site) %>%
	write_csv("data-summary/nested-naive.csv")

participants %>%
	filter(!pid %in% withdrawn$pid) %>%
	count(site, gender) %>%
	group_by(site) %>%
	mutate(p = n / sum(n), t = sum(n))