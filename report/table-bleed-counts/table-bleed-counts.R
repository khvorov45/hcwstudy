library(tidyverse)
library(kableExtra)

serology <- read_csv("data/serology.csv", col_types = cols())
vac_hist <- read_csv("data/vaccinations.csv", col_types = cols())

prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        .groups = "drop",
        prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
        prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
        prior2023 = sum(year >= 2018 & year < 2023 & (status == "Australia" | status == "Overseas")),
    )

serology %>% filter(pid %in% paste0("QCH-06", 5:9), year == 2020) %>% select(pid, year, day) %>% distinct()
serology %>% filter(pid %in% paste0("QCH-07", 0:1), year == 2020) %>% select(pid, year, day) %>% distinct()

serology %>% 
    filter(pid == "QCH-071", year == 2020, day %in% c(0, 14)) %>%
    pivot_wider(names_from = "day", values_from = "titre")

serology %>% 
    left_join(prior_vac_counts, "pid") %>%
    mutate(prior_study_year = case_when(
        year == 2020 ~ prior2020,
        year == 2021 ~ prior2021,
        year == 2022 ~ prior2022,
        year == 2023 ~ prior2023,
        TRUE ~ NA_integer_
    )) %>%
    select(pid, year, day, prior_study_year) %>%
    distinct() %>%
    count(day, year, prior_study_year) %>%
    mutate(across(c(year, prior_study_year, day), as.character)) %>%
    bind_rows(summarise(group_by(., day, year), n = sum(n), prior_study_year = "Total", .groups = "drop")) %>%
    bind_rows(summarise(group_by(., day, prior_study_year), n = sum(n), year = "Total", .groups = "drop")) %>%
    bind_rows(summarise(group_by(., year, prior_study_year), n = sum(n), day = "Total", .groups = "drop")) %>%
    mutate(day = fct_relevel(day, "0", "7")) %>%
    arrange(day, prior_study_year) %>%
    rename(Day = day, Year = year) %>%
    pivot_wider(names_from = "prior_study_year", values_from = "n", values_fill = 0) %>%
    kbl(
        format = "latex",
        caption = "Counts of antibody titres for each timepoint (day post-vaccination) 
        over the study years for different prior vaccination groups.",
        booktabs = TRUE,
        label = "routine-bleed-counts"
    ) %>%
    add_header_above(c(" " = 2, "Known vaccinations in the 5 years before bleed" = 6)) %>%
    column_spec(3:8, width = "1cm") %>%
    collapse_rows(columns = 1, latex_hline = "major") %>%
    write("report/table-bleed-counts/routine-bleed-counts.tex")

covid_serology <- read_csv("data/serology-covid.csv", col_types = cols())

covid_serology %>%
    mutate(day_cat = case_when(
        vax_inf == "I" ~ paste0("Post-infection day ", day),
        day >= 30 ~ paste0("Post-vaccination day 30+"),
        TRUE ~ paste0("Post-vaccination day ", day),
    )) %>%
    count(day_cat) %>%
    rename(`Day` = day_cat, Count = n) %>%
    kbl(
        format = "latex",
        caption = "Counts of covid antibody titres.",
        booktabs = TRUE,
        label = "bleed-counts-covid",
        linesep = c("", "", "\\addlinespace", "", "" ,"" ,"")
    ) %>%
    write("report/table-bleed-counts/routine-bleed-counts-covid.tex")
