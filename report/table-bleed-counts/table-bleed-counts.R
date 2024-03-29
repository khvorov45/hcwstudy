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

serology_for_counting <- serology %>% 
    left_join(prior_vac_counts, "pid") %>%
    mutate(prior_study_year = case_when(
        year == 2020 ~ prior2020,
        year == 2021 ~ prior2021,
        year == 2022 ~ prior2022,
        year == 2023 ~ prior2023,
        TRUE ~ NA_integer_
    )) %>%
    select(pid, year, day, prior_study_year, vax_inf) %>%
    distinct() %>%
    mutate(across(c(year, prior_study_year, day), as.character))

serology_for_counting %>%
    count(vax_inf, day, year, prior_study_year) %>%
    bind_rows(summarise(group_by(., vax_inf, day, year), n = sum(n), prior_study_year = "Total", .groups = "drop")) %>%
    bind_rows(summarise(group_by(., vax_inf, day, prior_study_year), n = sum(n), year = "Total", .groups = "drop")) %>%
    bind_rows(summarise(group_by(., vax_inf, year, prior_study_year), n = sum(n), day = "Total", .groups = "drop")) %>%
    bind_rows(summarise(group_by(serology_for_counting, vax_inf, day, prior_study_year), n = length(unique(pid)), year = "Subjects", .groups = "drop")) %>%
    bind_rows(summarise(group_by(serology_for_counting, vax_inf, day), n = length(unique(pid)),  prior_study_year = "Total", year = "Subjects", .groups = "drop")) %>%
    bind_rows(summarise(group_by(serology_for_counting, vax_inf, year, prior_study_year), n = length(unique(pid)), day = "Subjects", .groups = "drop")) %>%
    bind_rows(summarise(group_by(serology_for_counting, vax_inf, year), n = length(unique(pid)),  prior_study_year = "Total", day = "Subjects", .groups = "drop")) %>%
    bind_rows(summarise(group_by(serology_for_counting, vax_inf, prior_study_year), n = length(unique(pid)), day = "Subjects", year = "Subjects", .groups = "drop")) %>%
    bind_rows(summarise(group_by(serology_for_counting, vax_inf), n = length(unique(pid)),  prior_study_year = "Total", day = "Subjects", year = "Subjects", .groups = "drop")) %>%
    mutate(
        year = fct_relevel(year, "2020", "2021", "2022", "Total", "Subjects"),
        day = fct_relevel(day, "0", "7", "14", "30", "220", "Total", "Subjects"), 
        vax_inf = fct_relevel(vax_inf, "V") %>% recode("V" = "Vaccination", "I" = "Infection")
    ) %>%
    arrange(vax_inf, day, prior_study_year) %>%
    rename(Bleed = vax_inf, Day = day, Year = year) %>%
    pivot_wider(names_from = "prior_study_year", values_from = "n", values_fill = 0) %>%
    group_by(Bleed) %>%
    group_walk(function(subset, key) {
        subset %>%
            (function(x) {write_csv(x, glue::glue("report/table-bleed-counts/bleed-counts-{key$Bleed}.csv")); x}) %>%
            kbl(
                format = "latex",
                caption = glue::glue("Counts of {tolower(key$Bleed)} bleeds with measured antibody titres for each timepoint (day post-{tolower(key$Bleed)})
                over the study years for different prior vaccination groups.
                Rows labelled 'Total' sum all of the corresponding bleeds. 
                Rows labelled 'Subjects' count unique subjects in the corresponding categories."),
                booktabs = TRUE,
                label = glue::glue("bleed-counts-{key$Bleed}")
            ) %>%
            add_header_above(c(" " = 2, "Known vaccinations in the 5 years before bleed" = 6)) %>%
            column_spec(4:9, width = "1cm") %>%
            collapse_rows(columns = 1, latex_hline = "major") %>%
            # collapse_rows(columns = 2, latex_hline = "linespace") %>%
            write(glue::glue("report/table-bleed-counts/bleed-counts-{key$Bleed}.tex"))
    })

covid_serology <- read_csv("data/serology-covid.csv", col_types = cols())

covid_serology %>%
    filter(strain == "Wuhan") %>%
    mutate(
        day_cat = if_else(vax_inf == "V" & bleed_day_id > 0 & bleed_day_id < 220, 14, bleed_day_id) %>% as.character(),
        vax_inf = fct_relevel(vax_inf, "V") %>% recode("V" = "Vaccination", "I" = "Infection"),
    ) %>%
    count(vax_inf, day_cat) %>%
    bind_rows(summarise(group_by(., vax_inf), n = sum(n), day_cat = "Total", .groups = "drop")) %>%
    arrange(vax_inf) %>%
    rename(Bleed = vax_inf, `Day` = day_cat, Count = n) %>%
    (function(x) {write_csv(x, glue::glue("report/table-bleed-counts/routine-bleed-counts-covid.csv")); x}) %>%
    kbl(
        format = "latex",
        caption = "Counts of covid bleeds with measured antibody titres.",
        booktabs = TRUE,
        label = "bleed-counts-covid",
    ) %>%
    collapse_rows(columns = 1, latex_hline = "major") %>%
    write("report/table-bleed-counts/routine-bleed-counts-covid.tex")
