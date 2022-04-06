library(tidyverse)

participants <- read_csv("../data/participants.csv")

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

weekly_surveys <- read_csv("../data/weekly-surveys.csv") %>%
	left_join(participants %>% select(pid, site), "pid")

all_data <- list(
	participants = participants,
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
