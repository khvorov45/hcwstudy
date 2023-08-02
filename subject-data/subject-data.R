library(tidyverse)
library(kableExtra)

participants <- read_csv("data/participants.csv", col_types = cols())
weekly_surveys <- read_csv("data/weekly-surveys.csv", col_types = cols())

if (!dir.exists("subject-data/baseline")) {
    dir.create("subject-data/baseline")
}

baseline_tables <- participants %>%
    filter(pid == "ALF-001") %>%
    group_by(pid) %>%
    group_modify(function(pid_data, key) {
        strings <- pid_data %>%
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
        tibble(baseline_table = strings)
    }) %>%
    ungroup()

weekly_surveys_tables <- weekly_surveys %>%
    filter(pid == "ALF-001") %>%
    group_by(pid) %>%
    group_modify(function(pid_data, key) {
        strings <- pid_data %>%
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
        tibble(weekly_surveys_table = strings)
    }) %>%
    ungroup()

all_tables <- baseline_tables %>%
    full_join(weekly_surveys_tables, "pid")

template <- read_file("subject-data/reports/template.tex")

all_tables %>%
    filter(pid == "ALF-001") %>%
    group_by(pid) %>%
    group_walk(.keep = TRUE, function(pid_data, key) {
        pid_doc <- stringi::stri_replace_all_fixed(template, "PID", pid_data$pid) %>%
            stringi::stri_replace_all_fixed("BASELINETABLE", pid_data$baseline_table) %>%
            stringi::stri_replace_all_fixed("WEEKLYSURVEYSTABLE", pid_data$weekly_surveys_table)
        filename <- paste0("subject-data/reports/", pid_data$pid, ".tex")
        write_file(pid_doc, filename)
        cmd <- paste0("latexmk -pdf -outdir=subject-data/reports -silent ", filename)
        system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    })
