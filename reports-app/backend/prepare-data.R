suppressPackageStartupMessages(library(tidyverse))

vac_hist <- read_csv("./data/vaccinations.csv", col_types = cols())

prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
        prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
    )

study_year_vax_tbl <- vac_hist %>%
	filter(year >= 2020) %>%
	full_join(
		read_csv("./data/serology.csv", col_types = cols()) %>%
    		filter(day == 14) %>%
    		group_by(pid, year) %>%
    		summarise(.groups = "drop", d14sera = as.integer(any(!is.na(titre)))),
    	c("year", "pid")
	) %>%
	full_join(
		read_csv("./data/bleed-dates.csv", col_types = cols()) %>%
			rename(d14bleed_date = date) %>%
			filter(day == 14) %>%
			select(-day),
		c("year", "pid")
	) %>%
	mutate(study_year_vax = as.integer(
		(status == "Australia" | status == "Overseas") | d14sera == 1 | !is.na(d14bleed_date)
	))

study_year_vax_tbl_wide <- study_year_vax_tbl %>%
	select(pid, year, study_year_vax) %>%
	mutate(year = paste0("study_year_vac_", year)) %>%
	pivot_wider(names_from = "year", values_from = "study_year_vax") %>%
	mutate(across(contains("study_year_vac_"), ~replace_na(.x, 0)))

swabs <- read_csv("./data/swabs.csv", col_types = cols())

flu_positive_by_year <- swabs %>%
    group_by(pid, year) %>%
    summarise(.groups = "drop", positive = paste(swab_virus[swab_result == 1], collapse = ";")) %>%
    mutate(flu_positive = as.integer(str_detect(positive, "Flu"))) %>%
    select(-positive) %>%
    mutate(year = paste0("flupos", year)) %>%
    pivot_wider(names_from = "year", values_from = "flu_positive")

participants <- read_csv("./data/participants.csv", col_types = cols()) %>%
    mutate(age_group = cut(age_screening, c(-Inf, 18, 30, 50, 65), right = FALSE)) %>%
    left_join(prior_vac_counts, "pid") %>%
    left_join(
        read_csv("./data/consent.csv", col_types = cols()) %>%
            filter(
                (year == 2022 & disease == "flu") | (year == 2021 & disease == "covid"),
                !is.na(consent)
            ) %>%
            select(pid, disease, consent, date) %>%
            group_by(pid, disease) %>%
            summarise(
                .groups = "drop",
                consent = paste(unique(consent), collapse = ","),
                date = max(date)
            ) %>%
            mutate(disease = recode(disease, "flu" = "fluArm2022", "covid" = "covidArm2021")) %>%
            pivot_wider(names_from = "disease", values_from = c(consent, date)),
        "pid"
    ) %>%
    left_join(
        read_csv("./data/bleed-dates.csv", col_types = cols()) %>%
            filter(!is.na(date)) %>%
            group_by(pid) %>%
            summarise(
            	.groups = "drop",
            	bled2020 = as.integer(any(lubridate::year(date) == 2020)),
            	bled2021 = as.integer(any(lubridate::year(date) == 2021)),
            	bled2022 = as.integer(any(lubridate::year(date) == 2022)),
            ),
        "pid"
    ) %>%
    left_join(study_year_vax_tbl_wide, "pid") %>%
	mutate(
		across(contains("study_year_vac_"), ~replace_na(.x, 0)),
		across(contains("bled202"), ~replace_na(.x, 0)),
	) %>%
    left_join(flu_positive_by_year, "pid") %>%
    mutate(across(starts_with("flupos"), ~replace_na(.x, 0)))

withdrawn <- read_csv("./data/withdrawn.csv", col_types = cols()) %>%
    left_join(participants %>% select(pid, site), "pid") %>%
    rename(year = redcap_project_year)

bleed_dates_flu <- read_csv("./data/bleed-dates.csv", col_types = cols())

bleed_dates_flu_wide <- bleed_dates_flu %>%
    mutate(day = paste0("flu_day_", day)) %>%
    pivot_wider(names_from = "day", values_from = "date")

bleed_dates_covid <- read_csv("./data/covid-bleed-dates.csv", col_types = cols())

bleed_dates_covid_wide <- bleed_dates_covid %>%
    mutate(day = paste0("covid_day_", day)) %>%
    pivot_wider(names_from = "day", values_from = "date")

bleed_dates_wide <- full_join(bleed_dates_flu_wide, bleed_dates_covid_wide, c("pid", "year")) %>%
    left_join(participants, "pid")

postinf_bleed_dates <- read_csv("./data/postinf-bleed-dates.csv", col_types = cols()) %>%
    mutate(day = paste0("day", day)) %>%
    pivot_wider(names_from = "day", values_from = "bleed_date") %>%
    left_join(participants, "pid")

consent <- read_csv("./data/consent.csv", col_types = cols()) %>%
    left_join(participants %>% select(pid, site), "pid")

weekly_surveys <- read_csv("./data/weekly-surveys.csv", col_types = cols()) %>%
    left_join(participants %>% select(pid, site), "pid")

titres <- read_csv("./data/serology.csv", col_types = cols()) %>%
	left_join(participants %>% select(-contains("bled202"), -contains("Arm"), -email, -mobile), "pid")

#
# SECTION Data problems
#

all_data_problems_files <- list.files("data-problems") %>%
    .[!str_detect(., "-")] %>%
    paste0("data-problems/", .)

all_data_problems <- map(all_data_problems_files, ~ read_csv(.x, col_types = cols()))
names(all_data_problems) <- all_data_problems_files %>% str_replace("data-problems/(.*)\\.csv", "\\1")

#
# SECTION Full data
#

all_data <- c(list(
    participants = participants,
    withdrawn = withdrawn,
    bleed_dates = bleed_dates_wide,
    postinf_bleed_dates = postinf_bleed_dates,
    consent = consent,
    weekly_surveys = weekly_surveys,
    titres = titres
), all_data_problems)

write(jsonlite::toJSON(all_data), "./reports-app/backend/backend-all_data.json")

sites <- unique(participants$site)

for (site_name in sites) {
    all_site_data <- map(all_data, ~ filter(.x, site == site_name))
    write(jsonlite::toJSON(all_site_data), glue::glue("./reports-app/backend/backend-{site_name}.json"))
}
