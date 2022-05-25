suppressPackageStartupMessages(library(tidyverse))

vac_hist <- read_csv("./data/vaccinations.csv", col_types = cols())

prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
        prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
    )

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
            summarise(.groups = "drop", latestBleedDate = max(date)),
        "pid"
    )

withdrawn <- read_csv("./data/withdrawn.csv", col_types = cols()) %>%
    left_join(participants %>% select(pid, site), "pid") %>%
    rename(year = redcap_project_year)

bleed_dates_flu <- read_csv("./data/bleed-dates.csv", col_types = cols()) %>%
    left_join(participants %>% select(pid, site), "pid")

bleed_dates_flu_wide <- bleed_dates_flu %>%
    mutate(day = paste0("flu_day_", day)) %>%
    pivot_wider(names_from = "day", values_from = "date")

bleed_dates_covid <- read_csv("./data/covid-bleed-dates.csv", col_types = cols())

bleed_dates_covid_wide <- bleed_dates_covid %>%
    mutate(day = paste0("covid_day_", day)) %>%
    pivot_wider(names_from = "day", values_from = "date")

bleed_dates_wide <- full_join(bleed_dates_flu_wide, bleed_dates_covid_wide, c("pid", "year")) %>%
    left_join(participants, c("pid", "site"))

postinf_bleed_dates <- read_csv("./data/postinf-bleed-dates.csv", col_types = cols()) %>%
    mutate(day = paste0("day", day)) %>%
    pivot_wider(names_from = "day", values_from = "bleed_date") %>%
    left_join(participants, "pid")

consent <- read_csv("./data/consent.csv", col_types = cols()) %>%
    left_join(participants %>% select(pid, site), "pid")

weekly_surveys <- read_csv("./data/weekly-surveys.csv", col_types = cols()) %>%
    left_join(participants %>% select(pid, site), "pid")

titres <- read_csv("./data/serology.csv", col_types = cols()) %>%
	left_join(participants %>% select(-latestBleedDate, -contains("Arm"), -email, -mobile), "pid")

all_data <- list(
    participants = participants,
    withdrawn = withdrawn,
    bleed_dates = bleed_dates_wide,
    postinf_bleed_dates = postinf_bleed_dates,
    consent = consent,
    weekly_surveys = weekly_surveys,
    titres = titres
)

write(jsonlite::toJSON(all_data), "./reports-app/backend/backend-all_data.json")

sites <- unique(participants$site)

for (site_name in sites) {
    all_site_data <- map(all_data, ~ filter(.x, site == site_name))
    write(jsonlite::toJSON(all_site_data), glue::glue("./reports-app/backend/backend-{site_name}.json"))
}
