library(tidyverse)

vac_hist <- read_csv("./data/vaccinations.csv", col_types = cols())

prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        .groups = "drop",
        prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
    )

serology_all <- read_csv("data/serology.csv", col_types = cols()) %>%
    left_join(prior_vac_counts, "pid")

serology_vax2022 <- serology_all %>%
    filter(year == 2022, vax_inf == "V", virus_vaccine)

serology_vax2022 %>% count(virus, subtype)

ratios <- serology_vax2022 %>%
    select(pid, year, day, virus, titre, subtype, virus_egg_cell, prior2022) %>%
    pivot_wider(names_from = "day", values_from = "titre") %>%
    mutate(ratio = `14` / `0`, seroconv = if_else(`0` == 5, `14` >= 40, ratio >= 4)) %>%
    filter(!is.na(ratio))

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

bind_rows(
    summarise(ratios, .by = c(virus, subtype, virus_egg_cell), prior2022 = "total", summarise_prop(seroconv)),
    summarise(ratios, .by = c(virus, subtype, virus_egg_cell, prior2022), summarise_prop(seroconv)) %>% mutate(prior2022 = as.character(prior2022)),
) %>%
    select(virus, prior2022, prop_low_high) %>%
    pivot_wider(names_from = "virus", values_from = "prop_low_high") %>%
    left_join(
        bind_rows(
            summarise(ratios, prior2022 = "total", n = length(unique(pid))),
            summarise(ratios, .by = c(prior2022), n = length(unique(pid))) %>% mutate(prior2022 = as.character(prior2022)),
        ),
        "prior2022",
    ) %>%
    select(prior2022, n, contains("Victoria"), contains("Darwin"), contains("Austria")) %>%
    arrange(prior2022) %>%
    write_csv("leslie/summ.csv")

bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())
participants <- read_csv("data/participants.csv", col_types = cols())
swabs <- read_csv("data/swabs.csv", col_types = cols())

swabs_complete2022 <- swabs %>% 
    filter(year == 2022) %>%
    group_by(pid, year, postinf_instance) %>%
    filter(sum(swab_result) > 0) %>%
    ungroup() %>%
    filter(!is.na(samp_date)) %>%
    summarise(.by = c(pid, year, samp_date), result = paste0(swab_virus[swab_result == 1], collapse = ",")) %>%
    left_join(participants %>% select(pid, site), "pid")

contributing2022 <- serology_vax2022 %>%
    select(pid) %>%
    summarise(.by = pid, has_titre = TRUE) %>%
    full_join(bleed_dates %>% filter(year == 2022) %>% summarise(.by = pid, has_bleed_date = TRUE), "pid") %>%
    filter(has_titre | has_bleed_date) %>%
    select(pid) %>%
    left_join(participants %>% select(pid, site), "pid")

bind_rows(
    summarise(contributing2022, site = "total", contributing = n()),
    summarise(contributing2022, .by = site, contributing = n()),
) %>%
    inner_join(
        bind_rows(
            summarise(swabs_complete2022, site = "total", tested = n()),
            summarise(swabs_complete2022, .by = site, tested = n()),
        ),
        "site"
    ) %>%
    mutate(site = factor(site, c("melbourne", "sydney", "newcastle", "adelaide", "brisbane", "perth", "total"))) %>%
    arrange(site) %>%
    write_csv("leslie/swabs-summ.csv")

covidpos2022 <- swabs_complete2022 %>%
    filter(str_detect(result, "SARS-CoV-2")) 

bind_rows(
    summarise(covidpos2022, site = "total", covidpos = n()),
    summarise(covidpos2022, .by = site, covidpos = n()),
) %>%
    mutate(site = factor(site, c("melbourne", "sydney", "newcastle", "adelaide", "brisbane", "perth", "total"))) %>%
    arrange(site) %>%
    write_csv("leslie/covidpos.csv")

withdrawn <- read_csv("data/withdrawn.csv", col_types = cols())

withdrawn_for_sure <- withdrawn %>% 
    group_by(pid) %>% 
    filter(withdrawal_date == max(withdrawal_date)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    pull(pid)

participants_not_withdrawn <- participants %>% filter(!pid %in% withdrawn_for_sure)

summarise_factor <- function(arr) {
    missing <- paste0("missing", rlang::as_name(enquo(arr)))
    bind_cols(map(unique(arr), function(lvl) {
        count = sum(arr == lvl, na.rm = TRUE)
        total = sum(!is.na(arr))
        if (is.na(lvl)) {
            lvl <- missing
            count = sum(is.na(arr))
            total = length(arr)
        }
        res = tibble(.rows = 1)
        res[lvl] = paste0(count, " (", signif(count / total * 100, 2), "%)")
        res
    }))
}

bind_rows(
    summarise(participants_not_withdrawn, site = "total", n = n(), summarise_factor(gender)),
    summarise(participants_not_withdrawn, .by = c(site), n = n(), summarise_factor(gender)),
) %>%
    mutate(site = factor(site, c("melbourne", "sydney", "newcastle", "adelaide", "brisbane", "perth", "total"))) %>%
    arrange(site) %>%
    write_csv("leslie/genders.csv")

participants %>% filter(atsi == 1) %>% count(gender)

serology <- read_csv("data/serology.csv", col_types = cols())

bleed_dates %>%
    filter(year == 2022) %>%
    select(pid, day) %>%
    mutate(has_bleed_date = TRUE) %>%
    full_join(
        serology %>% filter(year == 2022) %>% select(pid, day) %>% mutate(has_titre = TRUE) %>% distinct(),
        c("pid", "day")
    ) %>%
    filter(has_bleed_date | has_titre) %>%
    select(pid, day) %>%
    summarise(
        in14 = sum(day == 14),
        in220 = sum(day == 220),
        in220from14 = sum(pid[day == 220] %in% pid[day == 14])
    )

bleed_dates %>%
    filter(year == 2022 | year == 2021) %>%
    select(pid, year) %>%
    distinct() %>%
    mutate(has_bleed_date = TRUE) %>%
    full_join(
        serology %>% filter(year == 2022 | year == 2021) %>% select(pid, year) %>% mutate(has_titre = TRUE) %>% distinct(),
        c("pid", "year")
    ) %>%
    filter(has_bleed_date | has_titre) %>%
    select(pid, year) %>%
    summarise(
        in2021 = sum(year == 2021),
        in2022 = sum(year == 2022),
        in2022from2021 = sum(pid[year == 2022] %in% pid[year == 2021])
    )

participants_bled2022 <- bleed_dates %>%
    filter(year == 2022) %>%
    select(pid) %>%
    mutate(has_bleed_date = TRUE) %>%
    full_join(
        serology %>% filter(year == 2022) %>% select(pid) %>% mutate(has_titre = TRUE) %>% distinct(),
        c("pid")
    ) %>%
    filter(has_bleed_date | has_titre) %>%
    select(pid) %>%
    distinct() %>%
    left_join(participants %>% select(pid, site, gender), "pid")

bind_rows(
    summarise(participants_bled2022, site = "total", n = n(), summarise_factor(gender)),
    summarise(participants_bled2022, .by = c(site), n = n(), summarise_factor(gender)),
) %>%
    mutate(site = factor(site, c("melbourne", "sydney", "newcastle", "adelaide", "brisbane", "perth", "total"))) %>%
    arrange(site) %>%
    write_csv("leslie/genders-bled2022.csv")

vac_hist %>%
    left_join(participants %>% select(pid, site, recruitment_year), "pid") %>%
    summarise(.by = c(pid, site), prior_at_recruitment = sum((year >= recruitment_year - 5) & (year < recruitment_year) & (status == "Australia" | status == "Overseas"))) %>%
    mutate(
        contributed_day7_in_2022 = pid %in% (
            bleed_dates %>% filter(year == 2022, day == 7) %>% pull(pid) %>% unique() %>%
                c(serology %>% filter(year == 2022, day == 7) %>% pull(pid) %>% unique())
        )
    ) %>%
    filter(prior_at_recruitment == 0, !pid %in% withdrawn_for_sure) %>%
    summarise(.by = site, contributed_day7_in_2022 = sum(contributed_day7_in_2022)) %>%
    mutate(site = factor(site, c("melbourne", "sydney", "newcastle", "adelaide", "brisbane", "perth", "total"))) %>%
    arrange(site) %>%
    write_csv("leslie/naive-nested.csv")

read_csv("data/participants.csv", col_types = cols()) %>%
    select(pid) %>%
    left_join(
        read_csv("data/bleed-dates.csv", col_types = cols()) %>%
            filter(year == 2023) %>%
            summarise(.by = pid, hasany = TRUE, has220 = 220 %in% day),
        c("pid")
    ) %>%
    mutate(hasany = replace_na(hasany, FALSE), has220 = replace_na(has220, FALSE), site = str_sub(pid, 1, 3)) %>%
    filter(hasany) %>%
    count(has220, site)

format_iqr <- function(x) {
    glue::glue("{round(median(x))} [{round(quantile(x, 0.25))}, {round(quantile(x, 0.75))}] [{round(min(x))}, {round(max(x))}]")
}

read_csv("data/participants.csv", col_types = cols()) %>%
    left_join(
        read_csv("data/covid-vax.csv", col_types = cols()) %>%
            filter(dose == 2) %>%
            select(pid, brand),
        "pid",
        relationship = "one-to-one"
    ) %>%
    left_join(
        read_csv("data/covid-vax.csv", col_types = cols()) %>%
            filter(dose %in% 1:2) %>%
            select(pid, dose, date) %>%
            pivot_wider(names_from = "dose", values_from = "date") %>%
            mutate(dose_1_2_interval = `2` - `1`) %>%
            select(pid, dose_1_2_interval),
        "pid",
        relationship = "one-to-one",
    ) %>%
    left_join(
        read_csv("data/covid-vax.csv", col_types = cols()) %>%
            filter(dose == 2) %>%
            select(pid, dose2_date = date) %>%
            left_join(
                read_csv("data/covid-bleed-dates.csv", col_types = cols()) %>%
                    select(pid, day, date) %>%
                    filter(day %in% c(7, 14)) %>%
                    filter(.by = c(pid, day), row_number() == 1) %>%
                    pivot_wider(names_from = "day", values_from = "date"),
                "pid",
                relationship = "one-to-one",
            ) %>%
            mutate(dose2_to_7 = `7` - dose2_date, dose2_to_14 = `14` - dose2_date) %>%
            select(pid, dose2_to_7, dose2_to_14),
        "pid",
        relationship = "one-to-one",
    ) %>%
    filter(pid %in% c("PCH-089", "PCH-097", "PCH-102", "PCH-129", "WCH-806", "ALF-806", "PCH-079", "PCH-180", "PCH-808", "PCH-822")) %>%
    summarise(
        .by = brand,
        age = format_iqr(age_screening),
        prop_female = glue::glue("{sum(gender == \"female\")} ({sum(gender == \"female\") / n() * 100}%)"),
        interval_dose1_2 = format_iqr(dose_1_2_interval),
        interval_dose2_day7 = format_iqr(dose2_to_7),
        interval_dose2_day14 = format_iqr(dose2_to_14),
    ) %>%
    print() %>%
    write_csv("temp.csv")
