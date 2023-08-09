library(tidyverse)
library(kableExtra)

weekly_surveys <- read_csv("data/weekly-surveys.csv", col_types = cols())
aris <- read_csv("symptom-duration/aris.csv", col_types = cols())
bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())
postinf_bleed_dates <- read_csv("data/postinf-bleed-dates.csv", col_types = cols())
postinf_comments <- read_csv("data/postinf-comments.csv", col_types = cols())
swabs <- read_csv("data/swabs.csv", col_types = cols()) %>%
    summarise(.by = c(pid, year, postinf_instance, samp_date), result = paste0(swab_virus[swab_result == 1], collapse = ", ")) %>%
    left_join(postinf_comments, c("pid", "year", "postinf_instance"))
covid_bleed_dates <- read_csv("data/covid-bleed-dates.csv", col_types = cols())
covid_vax <- read_csv("data/covid-vax.csv", col_types = cols())
comorbidities <- read_csv("data/comorbidities.csv", col_types = cols())
workdept <- read_csv("data/workdept.csv", col_types = cols())
participants <- read_csv("data/participants.csv", col_types = cols())
serology <- read_csv("data/serology.csv", col_types = cols())
serology_landscapes <- read_csv("data/serology-landscapes.csv", col_types = cols())
serology_covid <- read_csv("data/serology-covid.csv", col_types = cols())
serology_adenovirus <- read_csv("data/serology-adenovirus.csv", col_types = cols())
vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())
yearly_changes <- read_csv("data/yearly-changes.csv", col_types = cols()) %>%
    left_join(
        workdept %>%
            summarise(.by = c(pid, year), dept = paste0(dept, collapse = ", ")),
        c("pid", "redcap_project_year" = "year")
    ) %>%
    left_join(comorbidities %>%
        mutate(condition = recode(condition,
            "Cardiac disease" = "CD",
            "Renal disease" = "RD",
            "Chronic respiratory condition" = "CRC",
            "Haematological disorder" = "HD",
            "Chronic neurological condition" = "CNC",
            "Pregnancy" = "P",
            "Immunocompromising condition" = "IC",
            "Diabetes or other metabolic disorder" = "D",
            "Smoker" = "S",
            "None" = "N",
        )) %>%
        summarise(.by = c(pid, year), condition = paste0(condition[condition != "N"], collapse = ", ")),
        c("pid", "redcap_project_year" = "year")
    )

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
            caption = "Estimated ARI durations and corresponding swab results. Empty if no aris were reported on the surveys.
            I assumed multiple ARI reports from consecutive weeks are one ARI event.
            There is one row in the table per ARI event which may span multiple survey weeks.
            The procedure for estimating ARI duration is as follows:
            earliest weekly duration (from the first weekly survey for the ari event) +
            (if the ari event lasted for multiple weeks then count all the days between the first and the last week) +
            last daily duration (how many daily surveys in the last week of the ari event show symptoms).",
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

covid_bleed_dates_tables <- create_tables(covid_bleed_dates, function(pid_data) {
    pid_data %>%
        kbl(
            format = "latex",
            caption = "Covid bleed dates. Empty if no covid bleeds.",
            booktabs = TRUE,
            label = "covidbleeddates",
            escape = TRUE,
            col.names = c("Year", "Day", "Date", "Sample type")
        )
})

covid_vax_tables <- create_tables(covid_vax, function(pid_data) {
    pid_data %>%
        kbl(
            format = "latex",
            caption = "Covid vaccinations.",
            booktabs = TRUE,
            label = "covidvax",
            escape = TRUE,
            col.names = c("Dose", "Date", "Batch", "Brand")
        )
})

postinf_bleed_dates_tables <- create_tables(postinf_bleed_dates, function(pid_data) {
    pid_data %>%
        select(year, day, bleed_date, samp_date, swab_collection) %>%
        distinct() %>%
        kbl(
            format = "latex",
            caption = "Postinfection bleeds. Empty if no bleeds.",
            booktabs = TRUE,
            label = "postinfbleeds",
            escape = TRUE,
            col.names = c("Year", "Bleed day", "Bleed date", "Swab date", "Swab collected")
        )
})

swabs_tables <- create_tables(swabs, function(pid_data) {
    pid_data %>%
        select(year, samp_date, result, ari_swab_notes) %>%
        distinct() %>%
        kbl(
            format = "latex",
            caption = "Swab result. Empty if no swabs.",
            booktabs = TRUE,
            label = "swabs",
            escape = TRUE,
            col.names = c("Year", "Date", "Result", "Notes")
        )
})

serology_tables <- create_tables(serology, function(pid_data) {
    pid_data %>%
        select(-virus_egg_cell) %>%
        kbl(
            format = "latex",
            caption = "Flu serology results. \"Vax/inf\" indicates whether the bleed was post-vaccination (V) or post-infection (I).
            \"In vaccine\" indicates if a virus was in the southern hemisphere egg vaccine that year.",
            booktabs = TRUE,
            label = "serology",
            escape = TRUE,
            longtable = TRUE,
            col.names = c("Year", "Day", "Virus", "Titre", "Vax/Inf", "Subtype", "Clade", "In vaccine")
        )
})

serology_landscapes_tables <- create_tables(serology_landscapes, function(pid_data) {
    pid_data %>%
        select(year, day, virus, titre) %>%
        kbl(
            format = "latex",
            caption = "Extra H3 serology. Empty is not done for this participant.",
            booktabs = TRUE,
            label = "serology-landscapes",
            escape = TRUE,
            longtable = TRUE,
            col.names = c("Year", "Day", "Virus", "Titre")
        )
})

serology_covid_tables <- create_tables(serology_covid, function(pid_data) {
    pid_data %>%
        select(strain, bleed_day_id, ic50, vax_inf) %>%
        kbl(
            format = "latex",
            caption = "Covid serology results. Empty if not done for this participant.",
            booktabs = TRUE,
            label = "serology-covid",
            escape = TRUE,
            col.names = c("Strain", "Day", "IC50", "Vax/Inf")
        )
})

serology_adenovirus_tables <- create_tables(serology_adenovirus, function(pid_data) {
    pid_data %>%
        select(bleed_day_id, ic50) %>%
        kbl(
            format = "latex",
            caption = "Adenovirus serology results. Empty if not done for this participant.",
            booktabs = TRUE,
            label = "serology-adenovirus",
            escape = TRUE,
            col.names = c("Day", "IC50")
        )
})

vaccinations_tables <- create_tables(vaccinations, function(pid_data) {
    pid_data %>%
        select(year, status, vaccination_date, brand, batch) %>%
        arrange(year) %>%
        kbl(
            format = "latex",
            caption = "Vaccination history.",
            booktabs = TRUE,
            label = "vaccinations",
            escape = TRUE,
            col.names = c("Year", "Location", "Date", "Brand", "Batch")
        )
})

yearly_changes_tables <- create_tables(yearly_changes, function(pid_data) {
    pid_data %>%
        arrange(redcap_project_year) %>%
        select(redcap_project_year, children, emp_status, occupation, dept, condition) %>%
        mutate(condition = replace_na(condition, "")) %>%
        kbl(
            format = "latex",
            caption = "Characteristics that change yearly. Comborbitity key:
            Cardiac disease = CD,
            Renal disease = RD,
            Chronic respiratory condition = CRC,
            Haematological disorder = HD,
            Chronic neurological condition = CNC,
            Pregnancy = P,
            Immunocompromising condition = IC,
            Diabetes or other metabolic disorder = D,
            Smoker = S",
            booktabs = TRUE,
            label = "yearlychanges",
            escape = TRUE,
            col.names = c("Year", "Children", "Employment", "Occupation", "Department", "Condition")
        )
})

all_tables <- participants_tables %>%
    full_join(weekly_surveys_tables, "pid") %>%
    full_join(aris_tables, "pid") %>%
    full_join(bleed_dates_tables, "pid") %>%
    full_join(covid_bleed_dates_tables, "pid") %>%
    full_join(postinf_bleed_dates_tables, "pid") %>%
    full_join(swabs_tables, "pid") %>%
    full_join(serology_tables, "pid") %>%
    full_join(serology_landscapes_tables, "pid") %>%
    full_join(serology_covid_tables, "pid") %>%
    full_join(serology_adenovirus_tables, "pid") %>%
    full_join(vaccinations_tables, "pid") %>%
    full_join(yearly_changes_tables, "pid") %>%
    full_join(covid_vax_tables, "pid")

template <- read_file("subject-data/reports/template.tex")

all_tables %>%
    filter(pid %in% pids_to_run) %>%
    group_by(pid) %>%
    group_walk(.keep = TRUE, function(pid_data, key) {
        pid_doc <- stringi::stri_replace_all_fixed(template, "PID", pid_data$pid) %>%
            stringi::stri_replace_all_fixed("BASELINETABLE", pid_data$participants_table) %>%
            stringi::stri_replace_all_fixed("WEEKLYSURVEYSTABLE", pid_data$weekly_surveys_table) %>%
            stringi::stri_replace_all_fixed("ARISTABLE", pid_data$aris_table) %>%
            stringi::stri_replace_all_fixed("COVIDBLEEDDATESTABLE", pid_data$covid_bleed_dates_table) %>%
            stringi::stri_replace_all_fixed("FLUVAXTABLE", pid_data$vaccinations_table) %>%
            stringi::stri_replace_all_fixed("COVIDVAXTABLE", pid_data$covid_vax_table) %>%
            stringi::stri_replace_all_fixed("POSTINFBLEEDSTABLE", pid_data$postinf_bleed_dates_table) %>%
            stringi::stri_replace_all_fixed("SWABSTABLE", pid_data$swabs_table) %>%
            stringi::stri_replace_all_fixed("SEROLOGYTABLE", pid_data$serology_table) %>%
            stringi::stri_replace_all_fixed("SEROLOGYLANDSCAPETABLE", pid_data$serology_landscapes_table) %>%
            stringi::stri_replace_all_fixed("SEROLOGYCOVIDTABLE", pid_data$serology_covid_table) %>%
            stringi::stri_replace_all_fixed("SEROLOGYADENOVIRUSTABLE", pid_data$serology_adenovirus_table) %>%
            stringi::stri_replace_all_fixed("YEARLYCHANGESTABLE", pid_data$yearly_changes_table) %>%
            stringi::stri_replace_all_fixed("BLEEDDATESTABLE", pid_data$bleed_dates_table)
        filename <- paste0("subject-data/reports/", pid_data$pid, ".tex")
        write_file(pid_doc, filename)
        cmd <- paste0("latexmk -pdf -outdir=subject-data/reports -silent ", filename)
        system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)
    })
