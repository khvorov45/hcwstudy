library(tidyverse)
library(kableExtra)

vac_hist <- read_csv("data/vaccinations.csv", col_types = cols())

known_prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        .groups = "drop",
        known_prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas" | status == "No")),
        known_prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas" | status == "No")),
        known_prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas" | status == "No")),
        known_prior2023 = sum(year >= 2018 & year < 2023 & (status == "Australia" | status == "Overseas" | status == "No")),
    )

participants <- read_csv("data/participants.csv", col_types = cols())

known_prior_vac_counts %>%
    pivot_longer(-pid, names_to = "reference_year", values_to = "known_prior") %>%
    left_join(participants %>% select(pid, recruitment_year), "pid") %>%
    mutate(
        atleast0 = known_prior >= 0,
        atleast1 = known_prior >= 1,
        atleast2 = known_prior >= 2,
        atleast3 = known_prior >= 3,
        atleast4 = known_prior >= 4,
        atleast5 = known_prior >= 5,
    ) %>%
    select(-known_prior) %>%
    pivot_longer(starts_with("atleast"), names_to = "known_prior", values_to = "status") %>%
    mutate(
        known_prior = recode(known_prior, "atleast0" = "Total", "atleast5" = "All 5") %>%
            str_replace("atleast", "At least ") %>% 
            factor(c("All 5", "At least 4", "At least 3", "At least 2", "At least 1", "Total")),
        reference_year = str_replace(reference_year, "known_prior", ""),
        recruitment_year = as.character(recruitment_year),
    ) %>%
    group_by(reference_year, known_prior, recruitment_year) %>%
    summarise(n = sum(status), .groups = "drop") %>%
    bind_rows(summarise(group_by(., reference_year, known_prior), n = sum(n), recruitment_year = "Total", .groups = "drop")) %>%
    arrange(reference_year, known_prior) %>%
    pivot_wider(names_from = "recruitment_year", values_from = "n", values_fill = 0) %>%
    rename(`Ref year` = reference_year, `Prior years with known status` = known_prior) %>%
    kbl(
        format = "latex",
        caption = "Counts of participants split by the number of years for which we have their vaccination history. 
            \"Ref year\" is the reference year the 5 prior years' vaccination history relates to.
            For example, if the reference year is 2022 then the 5 years prior to that are 2021, 2020, 2019, 2018 and 2017.
            If we know someone's vaccination status in 2021, 2020, 2019 but not in 2018 and 2017 then they would have 3 \"prior years with known status\".
            This participant would then be counted in the \"Total\", \"At least 1\", \"At least 2\" and \"At least 3\" rows but not in \"At least 4\" and \"All 5\" rows.",
        booktabs = TRUE,
        label = "missing-vachist"
    ) %>%
    add_header_above(c(" " = 2, "Recruitment year" = 3)) %>%
    column_spec(3:5, width = "1cm") %>%
    column_spec(2, width = "2cm") %>%
    collapse_rows(columns = 1, latex_hline = "major") %>%
    write("report/missing-vachist/missing-vachist.tex")
