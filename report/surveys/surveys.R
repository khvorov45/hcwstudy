library(tidyverse)
library(kableExtra)

weekly <- read_csv("data/weekly-surveys.csv", col_types = cols())
daily <- read_csv("data/daily-surveys.csv", col_types = cols())
participants <- read_csv("data/participants.csv", col_types = cols())
withdrawn <- read_csv("data/withdrawn.csv", col_types = cols())
swabs <- read_csv("data/swabs.csv", col_types = cols())

weekly_survey_start_dates <- bind_rows(
    tibble(
        year = 2020,
        survey_index = 1:32,
        week_start = seq(lubridate::ymd("2020-04-06"), by = 7, length.out = 32)
    ),
    tibble(
        year = 2021,
        survey_index = 1:52,
        week_start = seq(lubridate::ymd("2021-01-04"), by = 7, length.out = 52)
    ),
    tibble(
        year = 2022,
        survey_index = 1:52,
        week_start = seq(lubridate::ymd("2022-01-03"), by = 7, length.out = 52)
    )
) %>% mutate(weeks_from_start = (week_start - min(week_start)) / lubridate::dweeks(1),)

weekly_filled <- weekly %>% filter(!is.na(ari), !is.na(respiratory), !is.na(systemic))

# NOTE(sen) This should include only the surveys each participant should have completed
survey_completions <- participants %>%
    group_by(pid, site, date_screening) %>%
    reframe(year = 2020:2022) %>%
    left_join(weekly_survey_start_dates, c("year"), multiple = "all") %>%
    filter(date_screening <= week_start) %>%
    left_join(
        withdrawn %>% 
            filter(!is.na(withdrawal_date)) %>% 
            group_by(pid) %>%
            # NOTE(sen) Be conservative and stop tracking surveys at the first withdrawal
            summarise(withdrawal_date = min(withdrawal_date), .groups = "drop"),
        c("pid")
    ) %>%
    filter(is.na(withdrawal_date) | withdrawal_date > week_start) %>%
    # NOTE(sen) Look at surveys from March-October only
    filter(month(week_start) >= 3, month(week_start) <= 10) %>%
    select(pid, site, year, survey_index) %>%
    left_join(
        weekly_filled %>% 
            select(pid, year, survey_index) %>%
            mutate(completed = TRUE),
        c("pid", "year", "survey_index")
    ) %>%
    mutate(completed = replace_na(completed, FALSE), year = as.character(year))

propsum <- function(completed, total) glue::glue("{signif(completed / total * 100, 2)}% ({completed}/{total})")

bind_rows(
    summarise(survey_completions, .by = c(site, year), prop = propsum(sum(completed), n())),
    summarise(survey_completions, .by = c(year), site = "Total", prop = propsum(sum(completed), n())),
    summarise(survey_completions, .by = c(site), year = "Total", prop = propsum(sum(completed), n())),
    summarise(survey_completions, .by = c(), site = "Total", year = "Total", prop = propsum(sum(completed), n())),
) %>%
    rename(Year = year, Site = site) %>%
    mutate(Site = tools::toTitleCase(Site) %>% fct_relevel("Total", after = 6L)) %>%
    arrange(Site) %>%
    pivot_wider(names_from = "Year", values_from = "prop") %>%
    write_csv("report/surveys/weekly-survey-completion.csv") %>%
    kbl(
        format = "latex",
        caption = "Completion of weekly surveys. Format: proprtion percentage (relevant/total).
        The surves included in the total count are only the surveys participants should have completed.
        To qualify as 'should have completed' the survey for any participant 
        must have been issued between the first week of March and the last week of October,
        after the participant was recruited and before the participant was withdrawn 
        (if they withdrew before the end of the study).",
        booktabs = TRUE,
        label = "weekly-survey-completion",
        linesep = c(
            "", "", "", "", "", "\\addlinespace"
        )
    ) %>%
    kable_styling(latex_options = "scale_down") %>%
    write("report/surveys/weekly-survey-completion.tex")

label_consecutive <- function(values) {
    result = c()
    next_label = 0
    current_index = 1
    next_is_the_same_seq = FALSE
    while (current_index <= length(values)) {
        if (current_index < length(values)) {
            this_value = values[current_index]
            next_value = values[current_index + 1]
            next_is_the_same_seq = next_value - this_value == 1
        }
        result = c(result, next_label)
        if (!next_is_the_same_seq) {
            next_label = next_label + 1
        }
        current_index = current_index + 1
    }
    result
}

label_consecutive(c(1))
label_consecutive(c(1, 2, 3, 60, 80, 81))

collapse_swab_results <- function(swab_results) {
    swab_results <- as.character(flatten(swab_results))
    result <- ""
    if (any(swab_results != "Negative" & swab_results != "No result")) {
        result <- paste0(unique(swab_results[swab_results != "Negative" & swab_results != "No result"]), collapse = ",")
    } else if (any(swab_results == "Negative")) {
        result <- "Negative"
    } else {
        result <- "No result"
    }
    if (str_detect(result, ",Negative")) {
        print(swab_results)
    }
    result
}

collapse_swab_results(list("Negative"))
collapse_swab_results(list("Negative", "No result"))
collapse_swab_results(list("Negative", "No result", "flu"))
collapse_swab_results(list("Negative", "No result", "flu", "covid"))
collapse_swab_results(list("No result", "flu", "covid"))
collapse_swab_results(list("Negative", "covid"))

swab_followups <- weekly %>%
    filter(ari == 1) %>%
    left_join(weekly_survey_start_dates, c("year", "survey_index")) %>%
    mutate(
        ari_date = if_else(is.na(date), week_start, date),
    ) %>%
    arrange(pid, weeks_from_start) %>%
    # NOTE(sen) Consecutive ari weeks should probably only have one swab so only keep
    # the first week from any consecutive group
    group_by(pid) %>%
    mutate(consecutive_ari_group = label_consecutive(weeks_from_start), diagnosis = replace_na(diagnosis, "na")) %>%
    group_by(pid, consecutive_ari_group) %>%
    summarise(
        .groups = "drop", 
        ari_date = min(ari_date), 
        year = unique(year), 
        anyflu = any(diagnosis == "flu"),
        anycovid = any(diagnosis == "covid"),
    ) %>%
    select(pid, year, ari_date, anyflu, anycovid) %>%
    left_join(
        swabs %>%
            group_by(pid, year, samp_date) %>%
            summarise(
                .groups = "drop",
                swab_result = if_else(sum(swab_result) == 0, list("Negative"), list(unique(swab_virus[swab_result == 1])))
            ),
        c("pid", "year"),
        multiple = "all",
    ) %>%
    arrange(pid, year, ari_date, samp_date) %>%
    mutate(
        swab_close_enough = (samp_date >= ari_date - 7) & (samp_date <= ari_date + 7),
        swab_close_enough = replace_na(swab_close_enough, FALSE),
    ) %>%
    group_by(pid, year, ari_date) %>%
    summarise(
        .groups = "drop", 
        swab_taken = sum(swab_close_enough) > 0,
        swab_result = collapse_swab_results(swab_result[swab_close_enough]),
        anyflu = unique(anyflu), 
        anycovid = unique(anycovid),
    ) %>%
    left_join(participants %>% select(pid, site), "pid") %>%
    mutate(year = as.character(year))

bind_rows(
    summarise(swab_followups, .by = c(site, year), prop = propsum(sum(swab_taken), n())),
    summarise(swab_followups, .by = c(year), site = "Total", prop = propsum(sum(swab_taken), n())),
    summarise(swab_followups, .by = c(site), year = "Total", prop = propsum(sum(swab_taken), n())),
    summarise(swab_followups, .by = c(), site = "Total", year = "Total", prop = propsum(sum(swab_taken), n())),
) %>%
    rename(Year = year, Site = site) %>%
    mutate(Site = tools::toTitleCase(Site) %>% fct_relevel("Total", after = 6L)) %>%
    arrange(Site, Year) %>%
    pivot_wider(names_from = "Year", values_from = "prop") %>%
    write_csv("report/surveys/weekly-survey-swab-followup.csv") %>%
    kbl(
        format = "latex",
        caption = "Proportions of weekly surveys that reported an ARI that were followed up by a swab.
        'Followed up by a swab' means that there is a swab dated to 7 days before the notification
        at the earliest and 7 days after the notification at the latest.
        Format: proprtion percentage (relevant/total). 
        If multiple consecutive surveys reported an ARI only the first one was taken.
        It was assumed that all of those consecutive surveys were reporting the same
        ARI event and so only one swab was expected to be taken.",
        booktabs = TRUE,
        label = "weekly-survey-swab-followup",
        linesep = c(
            "", "", "", "", "", "\\addlinespace"
        )
    ) %>%
    kable_styling(latex_options = "scale_down") %>%
    write("report/surveys/weekly-survey-swab-followup.tex")

swab_followups_flu <- swab_followups %>% filter(anyflu) %>%
    mutate(swab_result = fct_relevel(swab_result, 
        "Flu A (unsubtyped)", "Flu A H3", "Flu A (unsubtyped),Piconavirus",
        "Parainfluenza", "Other",
        "Negative", "No result"
    ))

bind_rows(
    summarise(swab_followups_flu, .by = c(site, year, swab_result), n = n()),
    summarise(swab_followups_flu, .by = c(site, swab_result), year = "Total", n = n()),
    summarise(swab_followups_flu, .by = c(year, swab_result), site  = "Total", n = n()),
    summarise(swab_followups_flu, .by = c(swab_result), site = "Total", year = "Total", n = n()),
) %>%
    rename(Year = year, Site = site) %>%
    mutate(Site = tools::toTitleCase(Site) %>% fct_relevel("Total", after = 6L)) %>%
    arrange(Site, Year) %>%
    pivot_wider(names_from = "Year", values_from = "n") %>%
    select(Site, swab_result, `2020`, `2021`, `2022`, Total) %>%
    mutate(across(starts_with("2"), ~replace_na(.x, 0))) %>%
    arrange(Site, swab_result) %>%
    write_csv("report/surveys/swab-followup-flu.csv") %>%
    kbl(
        format = "latex",
        caption = "Swab results for every weekly survey where the participant reported having seeked medical treatment and been diagnosed
        with flu. 'No result' means no swab result was recorded close enough (at most 7 days before or after) the weekly survey.
        If one swab has multiple positive results, those result are joined together and reported
        separately (e.g., a swab positive for flu and piconavirus would contibute to the `Flu,Piconavirus` row but not to
        the 'Flu' or 'Piconavirus' rows).",
        booktabs = TRUE,
        label = "weekly-survey-swab-followup-flu",
    ) %>%
    collapse_rows(columns = 1, latex_hline = "major") %>%
    write("report/surveys/swab-followup-flu.tex")

swab_followups_covid <- swab_followups %>% filter(anycovid) %>%
    mutate(swab_result = fct_relevel(swab_result, 
        "SARS-CoV-2", "SARS-CoV-2,Other", "Other",
        "Negative", "No result"
    ))

bind_rows(
    summarise(swab_followups_covid, .by = c(site, year, swab_result), n = n()),
    summarise(swab_followups_covid, .by = c(site, swab_result), year = "Total", n = n()),
    summarise(swab_followups_covid, .by = c(year, swab_result), site  = "Total", n = n()),
    summarise(swab_followups_covid, .by = c(swab_result), site = "Total", year = "Total", n = n()),
) %>%
    rename(Year = year, Site = site) %>%
    mutate(Site = tools::toTitleCase(Site) %>% fct_relevel("Total", after = 6L)) %>%
    arrange(Site, Year) %>%
    pivot_wider(names_from = "Year", values_from = "n") %>%
    select(Site, swab_result, `2020`, `2021`, `2022`, Total) %>%
    mutate(across(starts_with("2"), ~replace_na(.x, 0))) %>%
    arrange(Site, swab_result) %>%
    write_csv("report/surveys/swab-followup-covid.csv") %>%
    kbl(
        format = "latex",
        caption = "Swab results for every weekly survey where the participant reported having seeked medical treatment and been diagnosed
        with covid. 'No result' means no swab result was recorded close enough (at most 7 days before or after) the weekly survey.
        If one swab has multiple positive results, those result are joined together and reported
        separately (e.g., a swab positive for covid and piconavirus would contibute to the `covid,Piconavirus` row but not to
        the 'covid' or 'Piconavirus' rows).",
        booktabs = TRUE,
        label = "weekly-survey-swab-followup-covid",
    ) %>%
    collapse_rows(columns = 1, latex_hline = "major") %>%
    write("report/surveys/swab-followup-covid.tex")

swab_followups %>% 
    filter(anyflu) %>%
    count(swab_result)

weekly_filled_for_count <- weekly_filled %>%
    left_join(participants %>% select(pid, site), "pid") %>%
    mutate(year = as.character(year))

bind_rows(
    summarise(weekly_filled_for_count, .by = c(site, year), prop = propsum(sum(ari), n())),
    summarise(weekly_filled_for_count, .by = c(year), site = "Total", prop = propsum(sum(ari), n())),
    summarise(weekly_filled_for_count, .by = c(site), year = "Total", prop = propsum(sum(ari), n())),
    summarise(weekly_filled_for_count, .by = c(), site = "Total", year = "Total", prop = propsum(sum(ari), n())),
) %>%
    rename(Year = year, Site = site) %>%
    mutate(Site = tools::toTitleCase(Site) %>% fct_relevel("Total", after = 6L)) %>%
    arrange(Site, Year) %>%
    pivot_wider(names_from = "Year", values_from = "prop") %>%
    write_csv("report/surveys/weekly-survey-aris.csv") %>%
    kbl(
        format = "latex",
        caption = "Proportions of all filled weekly surveys that reported an ARI.
        Format: proprtion percentage (relevant/total).",
        booktabs = TRUE,
        label = "weekly-survey-aris",
        linesep = c(
            "", "", "", "", "", "\\addlinespace"
        )
    ) %>%
    kable_styling(latex_options = "scale_down") %>%
    write("report/surveys/weekly-survey-aris.tex")

bind_rows(
    summarise(weekly_filled_for_count, .by = c(site, year), prop = propsum(sum(diagnosis == "flu", na.rm = TRUE), n())),
    summarise(weekly_filled_for_count, .by = c(year), site = "Total", prop = propsum(sum(diagnosis == "flu", na.rm = TRUE), n())),
    summarise(weekly_filled_for_count, .by = c(site), year = "Total", prop = propsum(sum(diagnosis == "flu", na.rm = TRUE), n())),
    summarise(weekly_filled_for_count, .by = c(), site = "Total", year = "Total", prop = propsum(sum(diagnosis == "flu", na.rm = TRUE), n())),
) %>%
    rename(Year = year, Site = site) %>%
    mutate(Site = tools::toTitleCase(Site) %>% fct_relevel("Total", after = 6L)) %>%
    arrange(Site, Year) %>%
    pivot_wider(names_from = "Year", values_from = "prop") %>%
    write_csv("report/surveys/weekly-survey-aris-flu.csv") %>%
    kbl(
        format = "latex",
        caption = "Proportions of all filled weekly surveys that reported obtaining medical treatment and
        getting diagnosed with flu.
        Format: proprtion percentage (relevant/total).",
        booktabs = TRUE,
        label = "weekly-survey-aris-flu",
        linesep = c(
            "", "", "", "", "", "\\addlinespace"
        )
    ) %>%
    kable_styling(latex_options = "scale_down") %>%
    write("report/surveys/weekly-survey-aris-flu.tex")

bind_rows(
    summarise(weekly_filled_for_count, .by = c(site, year), prop = propsum(sum(diagnosis == "covid", na.rm = TRUE), n())),
    summarise(weekly_filled_for_count, .by = c(year), site = "Total", prop = propsum(sum(diagnosis == "covid", na.rm = TRUE), n())),
    summarise(weekly_filled_for_count, .by = c(site), year = "Total", prop = propsum(sum(diagnosis == "covid", na.rm = TRUE), n())),
    summarise(weekly_filled_for_count, .by = c(), site = "Total", year = "Total", prop = propsum(sum(diagnosis == "covid", na.rm = TRUE), n())),
) %>%
    rename(Year = year, Site = site) %>%
    mutate(Site = tools::toTitleCase(Site) %>% fct_relevel("Total", after = 6L)) %>%
    arrange(Site, Year) %>%
    pivot_wider(names_from = "Year", values_from = "prop") %>%
    write_csv("report/surveys/weekly-survey-aris-covid.csv") %>%
    kbl(
        format = "latex",
        caption = "Proportions of all filled weekly surveys that reported obtaining medical treatment and
        getting diagnosed with covid.
        Format: proprtion percentage (relevant/total).",
        booktabs = TRUE,
        label = "weekly-survey-aris-covid",
        linesep = c(
            "", "", "", "", "", "\\addlinespace"
        )
    ) %>%
    kable_styling(latex_options = "scale_down") %>%
    write("report/surveys/weekly-survey-aris-covid.tex")

weekly_filled_per_participant <- weekly_filled_for_count %>%
    mutate(diagnosis = replace_na(diagnosis, "na")) %>%
    summarise(
        .by = c(pid, year, site),
        everari = any(ari == 1),
        everflu = any(diagnosis == "flu"),
        evercovid = any(diagnosis == "covid"),
    )

bind_rows(
    summarise(weekly_filled_per_participant, .by = c(site, year), prop = propsum(sum(everari, na.rm = TRUE), n())),
    summarise(weekly_filled_per_participant, .by = c(year), site = "Total", prop = propsum(sum(everari, na.rm = TRUE), n())),
    summarise(weekly_filled_per_participant, .by = c(site), year = "Total", prop = propsum(sum(everari, na.rm = TRUE), n())),
    summarise(weekly_filled_per_participant, .by = c(), site = "Total", year = "Total", prop = propsum(sum(everari, na.rm = TRUE), n())),
) %>%
    rename(Year = year, Site = site) %>%
    mutate(Site = tools::toTitleCase(Site) %>% fct_relevel("Total", after = 6L)) %>%
    arrange(Site, Year) %>%
    pivot_wider(names_from = "Year", values_from = "prop") %>%
    write_csv("report/surveys/weekly-survey-aris-participants.csv") %>%
    kbl(
        format = "latex",
        caption = "Proportions of all participants (that filled at least one survey) 
        that reported an ARI on any of their surveys.
        Format: proprtion percentage (relevant/total).",
        booktabs = TRUE,
        label = "weekly-survey-aris-participants",
        linesep = c(
            "", "", "", "", "", "\\addlinespace"
        )
    ) %>%
    kable_styling(latex_options = "scale_down") %>%
    write("report/surveys/weekly-survey-aris-participants.tex")

bind_rows(
    summarise(weekly_filled_per_participant, .by = c(site, year), prop = propsum(sum(everflu, na.rm = TRUE), n())),
    summarise(weekly_filled_per_participant, .by = c(year), site = "Total", prop = propsum(sum(everflu, na.rm = TRUE), n())),
    summarise(weekly_filled_per_participant, .by = c(site), year = "Total", prop = propsum(sum(everflu, na.rm = TRUE), n())),
    summarise(weekly_filled_per_participant, .by = c(), site = "Total", year = "Total", prop = propsum(sum(everflu, na.rm = TRUE), n())),
) %>%
    rename(Year = year, Site = site) %>%
    mutate(Site = tools::toTitleCase(Site) %>% fct_relevel("Total", after = 6L)) %>%
    arrange(Site, Year) %>%
    pivot_wider(names_from = "Year", values_from = "prop") %>%
    write_csv("report/surveys/weekly-survey-aris-flu-participants.csv") %>%
    kbl(
        format = "latex",
        caption = "Proportions of all participants (that filled at least one survey) 
        that reported getting medical treatment and diagnosed with flu on any of their surveys.
        Format: proprtion percentage (relevant/total).",
        booktabs = TRUE,
        label = "weekly-survey-aris-flu-participants",
        linesep = c(
            "", "", "", "", "", "\\addlinespace"
        )
    ) %>%
    kable_styling(latex_options = "scale_down") %>%
    write("report/surveys/weekly-survey-aris-flu-participants.tex")

bind_rows(
    summarise(weekly_filled_per_participant, .by = c(site, year), prop = propsum(sum(evercovid, na.rm = TRUE), n())),
    summarise(weekly_filled_per_participant, .by = c(year), site = "Total", prop = propsum(sum(evercovid, na.rm = TRUE), n())),
    summarise(weekly_filled_per_participant, .by = c(site), year = "Total", prop = propsum(sum(evercovid, na.rm = TRUE), n())),
    summarise(weekly_filled_per_participant, .by = c(), site = "Total", year = "Total", prop = propsum(sum(evercovid, na.rm = TRUE), n())),
) %>%
    rename(Year = year, Site = site) %>%
    mutate(Site = tools::toTitleCase(Site) %>% fct_relevel("Total", after = 6L)) %>%
    arrange(Site, Year) %>%
    pivot_wider(names_from = "Year", values_from = "prop") %>%
    write_csv("report/surveys/weekly-survey-aris-covid-participants.csv") %>%
    kbl(
        format = "latex",
        caption = "Proportions of all participants (that filled at least one survey) 
        that reported getting medical treatment and diagnosed with covid on any of their surveys.
        Format: proprtion percentage (relevant/total).",
        booktabs = TRUE,
        label = "weekly-survey-aris-covid-participants",
        linesep = c(
            "", "", "", "", "", "\\addlinespace"
        )
    ) %>%
    kable_styling(latex_options = "scale_down") %>%
    write("report/surveys/weekly-survey-aris-covid-participants.tex")
