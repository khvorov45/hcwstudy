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
    summarise(.by = c(reference_year, known_prior, recruitment_year), n = sum(status)) %>%
    bind_rows(summarise(., .by = c(reference_year, known_prior), n = sum(n), recruitment_year = "Total")) %>%
    mutate(.by = c(reference_year, recruitment_year), n = paste0(n, " (", round(n / n[known_prior == "Total"] * 100), "%)")) %>%
    arrange(reference_year, known_prior) %>%
    pivot_wider(names_from = "recruitment_year", values_from = "n", values_fill = "0") %>%
    rename(`Ref year` = reference_year, `Prior years with known status` = known_prior) %>%
    write_csv("report/missing-vachist/missing-vachist.csv") %>%
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
    kable_styling(latex_options = "scale_down") %>%
    add_header_above(c(" " = 2, "Recruitment year" = 4)) %>%
    column_spec(3:7, width = "2.5cm") %>%
    column_spec(2, width = "2cm") %>%
    collapse_rows(columns = 1, latex_hline = "major") %>%
    write("report/missing-vachist/missing-vachist.tex")

vac_hist_all_combos <- vac_hist %>%
    select(pid, year, status) %>%
    arrange(year) %>%
    pivot_wider(names_from = "year", values_from = "status") %>%
    count(pick(starts_with("2"))) %>%
    rename(Count = n) %>%
    mutate(across(starts_with("2"), ~replace_na(.x, "Not recorded"))) %>%
    arrange(desc(Count))

vac_hist_all_combos %>%
    filter(Count >= 10) %>%
    write_csv("report/missing-vachist/vachist-combos.csv") %>%
    kbl(
        format = "latex",
        caption = glue::glue(
            "Counts of the most common (represented by at least 10 subjects) vaccination history combinations.
            The table accounts for {vac_hist_all_combos %>% filter(Count >= 10) %>% pull(Count) %>% sum()} out of
            the {vac_hist_all_combos %>% pull(Count) %>% sum()} total subjects."
        ),
        booktabs = TRUE,
        label = "vachist-combos"
    ) %>%
    kable_styling(latex_options = "scale_down") %>%
    str_replace_all("Not recorded", "\\\\textcolor{gray}{(missing)}") %>%
    write("report/missing-vachist/vachist-combos.tex")
