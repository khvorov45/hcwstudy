library(tidyverse)

survey_dates <- tibble(
    year = 2020,
    survey_week = 1:32,
    survey_end_date = seq(lubridate::ymd("2020-04-12"), by = 7, length.out = 32)
) %>%
    bind_rows(tibble(
        year = 2021,
        survey_week = 1:52,
        survey_end_date = seq(lubridate::ymd("2021-01-10"), by = 7, length.out = 52)
    )) %>%
    bind_rows(tibble(
        year = 2022,
        survey_week = 1:52,
        survey_end_date = seq(lubridate::ymd("2022-01-09"), by = 7, length.out = 52)
    )) 

sercovid_init <- read_csv("data/serology-covid.csv", col_types = cols())
covid_vax <- read_csv("data/covid-vax.csv", col_types = cols())
swabs <- read_csv("data/swabs.csv", col_types = cols())
postinf_comments <- read_csv("data/postinf-comments.csv", col_types = cols())
postinf_bleeds <- read_csv("data/postinf-bleed-dates.csv", col_types = cols())
participants <- read_csv("data/participants.csv", col_types = cols()) %>%
    mutate(
        agegroup = cut(age_screening, c(-Inf, 20, 30, 40, 50, Inf)),
        bmigroup = cut(bmi, c(-Inf, 25, 30, Inf)),
    )

earliest_covid_infections_swabs <- swabs %>%
    filter(swab_result == 1, swab_virus == "SARS-CoV-2") %>%
    select(-swab_result, -swab_virus) %>%
    group_by(pid) %>%
    filter(samp_date == min(samp_date)) %>%
    ungroup() %>%
    rename(earliest_covid = samp_date)

postinf_comments_relevant <- postinf_comments %>%
    mutate(ari_swab_notes = tolower(ari_swab_notes)) %>%
    filter(str_detect(ari_swab_notes, "covid") | str_detect(ari_swab_notes, "rat")) %>%
    filter(str_detect(ari_swab_notes, "pos ") | str_detect(ari_swab_notes, "positive")) %>%
    filter(
        !(postinf_instance %in% c(28975, 41923, 42401, 48249, 48394, 94393, 57486))
        #!str_detect(ari_swab_notes, "positive for rhinovirus"), 
        #!str_detect(ari_swab_notes, ""), 
    ) %>%
    left_join(survey_dates, c("year", "survey_week")) %>%
    mutate(event = if_else(is.na(survey_week), "infection", "survey")) %>%
    group_by(pid, event) %>%
    filter(is.na(survey_end_date) | survey_end_date == min(survey_end_date)) %>%
    ungroup() %>%
    filter(!postinf_instance %in% earliest_covid_infections_swabs$postinf_instance) %>%
    select(pid, year, survey_week, postinf_instance, survey_end_date, event, ari_swab_notes)

earliest_covid_infections_comments_surveys <- postinf_comments_relevant %>%
    filter(event == "survey") %>%
    group_by(pid) %>%
    filter(survey_end_date == min(survey_end_date)) %>%
    ungroup() %>%
    select(pid, year, postinf_instance, earliest_covid = survey_end_date)

postinf_bleeds_ealiest_per_event <- postinf_bleeds %>%
    group_by(pid, year, postinf_instance) %>%
    filter(bleed_date == min(bleed_date)) %>%
    ungroup()

earliest_covid_infections_comments_infections_bleeds <- postinf_comments_relevant %>%
    filter(event == "infection") %>%
    left_join(postinf_bleeds_ealiest_per_event, c("pid", "year", "postinf_instance")) %>%
    group_by(pid) %>%
    filter(n() == 1 | bleed_date == min(bleed_date)) %>%
    ungroup() %>%
    filter(!is.na(bleed_date)) %>%
    mutate(earliest_covid = bleed_date - day) %>%
    select(pid, year, postinf_instance, earliest_covid)

earliest_covid_infections_comments_infections_comments <- postinf_comments_relevant %>%
    filter(!postinf_instance %in% c(earliest_covid_infections_comments_surveys$postinf_instance, earliest_covid_infections_comments_infections_bleeds$postinf_instance)) %>%
    mutate(comment_date = str_extract(ari_swab_notes, "(\\d\\d?/\\d\\d?/\\d\\d\\d*)") %>% lubridate::dmy()) %>%
    select(pid, year, postinf_instance, earliest_covid = comment_date)

earliest_covid <- bind_rows(
    earliest_covid_infections_swabs,
    earliest_covid_infections_comments_surveys,
    earliest_covid_infections_comments_infections_bleeds,
    earliest_covid_infections_comments_infections_comments,
) %>%
    group_by(pid) %>%
    filter(earliest_covid == min(earliest_covid)) %>%
    filter(row_number() == 1) %>%
    ungroup()

covid_vax_wide <- covid_vax %>% 
    select(pid, dose, date, brand) %>%
    mutate(dose = paste0("dose", dose)) %>%
    pivot_wider(names_from = "dose", values_from = c("date", "brand"))

sercovid <- sercovid_init %>%
    filter(vax_inf == "V") %>%
    select(-vax_inf) %>%
    rename(blood_sample_date = date) %>%
    left_join(covid_vax_wide, "pid") %>%
    mutate(
        days_from_dose2 = (blood_sample_date - date_dose2) / lubridate::ddays(1),
        timepoint = if_else(days_from_dose2 > 0, "Post-vax", "Pre-vax") %>% factor(c("Pre-vax", "Post-vax")),
    ) %>%
    filter(!is.na(days_from_dose2)) %>%
    # NOTE(sen) Discard any bleed that had a covid infection before it since we are looking at vaccination effect here
    left_join(earliest_covid %>% select(pid, earliest_covid), "pid") %>%
    mutate(nocovid = is.na(earliest_covid)) %>%
    group_by(pid, nocovid) %>%
    filter(is.na(earliest_covid) | blood_sample_date < earliest_covid) %>%
    ungroup() %>%
    select(-nocovid) %>%
    # NOTE(sen) Discard mixed brands
    filter(brand_dose1 == brand_dose2) %>%
    mutate(brand = brand_dose1) %>%
    # NOTE(sen) Keep only the post bleeds that are at least 14 days after dose 2
    filter(days_from_dose2 <= 0 | days_from_dose2 >= 14) %>%    
    # NOTE(sen) Discard bleeds after the third/forth doses
    filter(is.na(date_dose3) | blood_sample_date < date_dose3) %>%
    filter(is.na(date_dose4) | blood_sample_date < date_dose4) %>%    
    # NOTE(sen) Keep only the first bleed before/after dose2
    mutate(before_dose_2 = days_from_dose2 < 0) %>%
    group_by(pid, before_dose_2) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    # NOTE(sen) Remove the ones with high baseline (likely an infection we didn't catch)
    filter(!before_dose_2 | ic50 < 10) %>%    
    # NOTE(sen) Keep only the ones with pre/post bleeds
    group_by(pid) %>%
    filter(n() == 2) %>%
    ungroup() %>%
    left_join(participants, "pid")

# NOTE(sen) There isn't much of a time trend with days_from_dose2 so I'm not plotting it.
# Also, (almost) everyone seems to start from ic50=1, so I'm not plotting baseline titres.

sercovid_plot <- sercovid %>%
    filter(days_from_dose2 > 0) %>%
    ggplot(aes(brand, ic50)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    scale_y_log10("IC50 post-vax", breaks = c(1, 5, 10, 50, 100, 200, 300, 400, 500)) +
    scale_x_discrete("Brand") +
    geom_point(shape = 18, position = position_jitter(width = 0.1), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.alpha = 0, width = 0.3)

ggsave("covid-serology/ic50.pdf", sercovid_plot, width = 10, height = 10, units = "cm")

sercovid_bmi_plot <- sercovid %>%
    filter(days_from_dose2 > 0) %>%
    ggplot(aes(brand, ic50, color = bmigroup)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    scale_y_log10("IC50 post-vax", breaks = c(1, 5, 10, 50, 100, 200, 300, 400, 500)) +
    scale_x_discrete("Brand") +
    geom_point(shape = 18, position = position_dodge(width = 1), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.alpha = 0, width = 0.3, position = position_dodge(width = 1))

ggsave("covid-serology/ic50-bmi.pdf", sercovid_bmi_plot, width = 10, height = 10, units = "cm")

sercovid_age_plot <- sercovid %>%
    filter(days_from_dose2 > 0) %>%
    ggplot(aes(brand, ic50, color = agegroup)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    scale_y_log10("IC50 post-vax", breaks = c(1, 5, 10, 50, 100, 200, 300, 400, 500)) +
    scale_x_discrete("Brand") +
    geom_point(shape = 18, position = position_dodge(width = 1), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.alpha = 0, width = 0.3, position = position_dodge(width = 1))

ggsave("covid-serology/ic50-age.pdf", sercovid_age_plot, width = 10, height = 10, units = "cm")

sercovid_sex_plot <- sercovid %>%
    filter(days_from_dose2 > 0) %>%
    ggplot(aes(brand, ic50, color = gender)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    scale_y_log10("IC50 post-vax", breaks = c(1, 5, 10, 50, 100, 200, 300, 400, 500)) +
    scale_x_discrete("Brand") +
    geom_point(shape = 18, position = position_dodge(width = 1), alpha = 0.5) +
    geom_boxplot(fill = NA, outlier.alpha = 0, width = 0.3, position = position_dodge(width = 1))

ggsave("covid-serology/ic50-sex.pdf", sercovid_sex_plot, width = 10, height = 10, units = "cm")
