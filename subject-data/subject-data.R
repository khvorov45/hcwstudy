library(tidyverse)
library(kableExtra)

participants <- read_csv("data/participants.csv", col_types = cols())
weekly_surveys <- read_csv("data/weekly-surveys.csv", col_types = cols())
aris <- read_csv("symptom-duration/aris.csv", col_types = cols())
bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())

pids_to_run <- participants$pid[1:10]

create_tables <- function(data, kblfun) {
    data_name <- deparse(substitute(data))
    varname <- paste0(data_name, "_table")
    data %>%
        mutate(have_data = TRUE) %>%
        full_join(participants %>% select(pid), "pid") %>%
        filter(pid %in% pids_to_run) %>%
        group_by(pid) %>%
        group_modify(function(pid_data, key) {
            strings <- pid_data %>%
                filter(!is.na(have_data)) %>%
                select(-have_data) %>%
                kblfun()
            tibble(!!rlang::sym(varname) := as.character(strings))
        }) %>%
        ungroup()
}

participants_tables <- create_tables(participants, function(pid_data) {
    pid_data %>%
        select(date_screening, gender, dob, atsi, height, weight, years_employed) %>%
        kbl(
            format = "latex",
            caption = "Baseline characteristics.",
            booktabs = TRUE,
            label = "baseline",
            escape = TRUE,
            col.names = c("Screening", "Gender", "Date of Birth", "Aboriginal status", "Height (cm)", "Weight (kg)", "Years employed")
        ) %>%
        kable_styling(latex_options = "scale_down")
})

weekly_surveys_tables <- create_tables(weekly_surveys, function(pid_data) {
    pid_data %>%
        filter(if_any(c(n_respiratory, n_systemic, ari), ~(.x != 0) & (!is.na(.x)))) %>%
        select(year, date, ari, n_respiratory, n_systemic, fever, chills, headache, myalgia, malaise, cough, sorethroat, runnynose, chestpain, breathing, symptom_duration, diagnosis, comments) %>%
        kbl(
            format = "latex",
            caption = "Weekly surveys that reported symptoms. Empty when no surveys reported symptoms.",
            booktabs = TRUE,
            label = "weeklysurveys",
            escape = TRUE,
            col.names = c("Year", "Date", "ARI", "# Resp. symptoms", "# Systemic symptoms", "Fever", "Chills", "Headache", "Myalgia", "Malaise", "Cough", "Sorethroat", "Runnynose", "Chestpain", "Breathing", "Symptom duration", "Diagnosis", "Comments")
        ) %>%
        kable_styling(latex_options = "scale_down")
})

aris_tables <- create_tables(aris, function(pid_data) {
    pid_data %>%
        select(year, ari_start, total_known_symptom_duration, swab_date, swab_result) %>%
        kbl(
            format = "latex",
            caption = "Estimated ARI durations and corresponding swab results. Empty if no aris were reported on the surveys.",
            booktabs = TRUE,
            label = "aris",
            escape = TRUE,
            col.names = c("Year", "Start", "Duration (days)", "Swabbed", "Result")
        )
})

bleed_dates_tables <- create_tables(bleed_dates, function(pid_data) {
    pid_data %>%
        kbl(
            format = "latex",
            caption = "Bleed dates",
            booktabs = TRUE,
            label = "bleeddates",
            escape = TRUE,
            col.names = c("Year", "Day", "Date", "Sample type")
        )
})

all_tables <- baseline_tables %>%
    full_join(weekly_surveys_tables, "pid") %>%
    full_join(aris_tables, "pid") %>%
    full_join(bleed_dates_tables, "pid")

template <- read_file("subject-data/reports/template.tex")

all_tables %>%
    filter(pid %in% pids_to_run) %>%
    group_by(pid) %>%
    group_walk(.keep = TRUE, function(pid_data, key) {
        pid_doc <- stringi::stri_replace_all_fixed(template, "PID", pid_data$pid) %>%
            stringi::stri_replace_all_fixed("BASELINETABLE", pid_data$baseline_table) %>%
            stringi::stri_replace_all_fixed("WEEKLYSURVEYSTABLE", pid_data$weekly_surveys_table) %>%
            stringi::stri_replace_all_fixed("ARISTABLE", pid_data$aris_table) %>%
            stringi::stri_replace_all_fixed("BLEEDDATESTABLE", pid_data$bleed_dates_table)
        filename <- paste0("subject-data/reports/", pid_data$pid, ".tex")
        write_file(pid_doc, filename)
        cmd <- paste0("latexmk -pdf -outdir=subject-data/reports -silent ", filename)
        system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)
    })
