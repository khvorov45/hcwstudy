library(tidyverse)

vac_hist <- read_csv("../data/vaccinations.csv")

prior_vac_counts <- vac_hist %>%
	group_by(pid) %>%
	summarise(
		prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
		prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
		prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
	)

participants <- read_csv("../data/participants.csv") %>%
	mutate(age_group = cut(age_screening, c(-Inf, 18, 30, 50, 65), right = FALSE)) %>%
	left_join(prior_vac_counts, "pid")

withdrawn <- read_csv("../data/withdrawn.csv") %>%
	left_join(participants %>% select(pid, site), "pid") %>%
	rename(year = redcap_project_year)

bleed_dates_flu <- read_csv("../data/bleed-dates.csv") %>%
	left_join(participants %>% select(pid, site), "pid")

bleed_dates_flu_wide <- bleed_dates_flu %>%
	mutate(day = paste0("flu_day_", day)) %>%
	pivot_wider(names_from = "day", values_from = "date")

bleed_dates_covid <- read_csv("../data/covid-bleed-dates.csv")

bleed_dates_covid_wide <- bleed_dates_covid %>%
	mutate(day = paste0("covid_day_", day)) %>%
	pivot_wider(names_from = "day", values_from = "date")

bleed_dates_wide <- full_join(bleed_dates_flu_wide, bleed_dates_covid_wide, c("pid", "year"))

consent <- read_csv("../data/consent.csv") %>%
	left_join(participants %>% select(pid, site), "pid")

covid_arms <- consent %>%
	filter(disease == "covid", !is.na(consent), consent != "no") %>%
	group_by(pid) %>%
	#filter(length(na.omit(consent)) == 1 | form == "electronic") %>%
	summarise(covid_arm = paste(na.omit(consent), collapse = ",")) #%>%
	#mutate(covid_arm = if_else(covid_arm == "", "no", covid_arm))

weekly_surveys <- read_csv("../data/weekly-surveys.csv") %>%
	left_join(participants %>% select(pid, site), "pid")

all_data <- list(
	participants = participants %>% left_join(covid_arms, "pid"),
	withdrawn = withdrawn,
	bleed_dates = bleed_dates_wide,
	consent = consent,
	weekly_surveys = weekly_surveys
)

write(jsonlite::toJSON(all_data), "backend/backend-all_data.json")

sites <- unique(participants$site)

for (site_name in sites) {
	all_site_data <- map(all_data, ~filter(.x, site == site_name))
	write(jsonlite::toJSON(all_site_data), glue::glue("backend/backend-{site_name}.json"))
}
