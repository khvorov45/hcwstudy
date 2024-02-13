library(tidyverse)
library(kableExtra)

all_files <- list.files("subject-data/reports", full.names = TRUE)
file.remove(all_files[!str_ends(all_files, "template.tex")])

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
serology <- read_csv("data/serology.csv", col_types = cols(subtype = col_factor(levels = c("H1", "H3", "BVic", "BYam"))))
serology_landscapes <- read_csv("data/serology-landscapes.csv", col_types = cols(virus = col_factor(levels = c(
    "A/Bilthoven/16190/68",
    "A/Bilthoven/21793/72",
    "A/Bilthoven/1761/76",
    "A/Bilthoven/2271/76",
    "A/Philippines/2/82",
    "A/Netherlands/620/89",
    "A/Netherlands/179/93",
    "A/Netherlands/178/95",
    "A/Townsville/2/1999p",
    "A/Tasmania/1/97",
    "A/Philippines/472/02p",
    "A/Victoria/511/2004p",
    "A/URUGUAY/716/2007e",
    "A/Brisbane/10/2007p",
    "A/Perth/16/2009e",
    "A/Perth/16/2009",
    "A/Victoria/361/2011e",
    "A/Victoria/361/2011p",
    "A/Texas/50/2012e",
    "A/Texas/50/2012p",
    "A/Switzerland/9715293/13e",
    "A/Switzerland/9715293/13",
    "A/New Caledonia/104/2014p",
    "A/Hong Kong/4801/14e",
    "A/New Castle/30/2016",
    "A/Singapore/16-0019/16e",
    "A/Kansas/14/2017e",
    "A/Kansas/14/2017",
    "A/Switzerland/8060/2017e",
    "A/Brisbane/117/2018",
    "A/Brisbane/60/2018",
    "A/South Australia/34/2019",
    "A/South Australia/34/2019e",
    "A/Darwin/726/2019",
    "A/Hong Kong/2671/2019egg IVR",
    "A/Cambodia/e0826360/2020",
    "A/Darwin/9/2021e",
    "A/Darwin/6/2021"
))))
serology_covid <- read_csv("data/serology-covid.csv", col_types = cols())
serology_adenovirus <- read_csv("data/serology-adenovirus.csv", col_types = cols())
vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())
yearly_changes <- read_csv("data/yearly-changes.csv", col_types = cols()) %>%
    left_join(
        workdept %>%
            mutate(dept = recode(dept, 
                "Emergency Department" = "ED",
                "Critical Care or Intensive Care Unit" = "ICU",
                "General Medicine and/or Medical Specialities" = "GM",
                "Paediatrics and/or Paediatric Specialities" = "PD",
                "Surgery and/or Surgical Specialties" = "SG",
                "Gynaecology and/or Obstetrics" = "GN",
                "Oncology and/or Haematology" = "ON",
                "Radiology" = "RD",
                "Outpatient clinic" = "OC",
                "Pharmacy" = "PH",
                "Laboratory" = "LB",
                "Nutrition" = "NT",
                "Social Work" = "SW",
                "Physiotherapy" = "PT",
                "Occupational therapy" = "OT",
                "Other" = "O",
            )) %>%
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

pids_to_run <- c(participants$pid[1:9], "ALF-085", "JHH-809")

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
            longtable = TRUE,
            col.names = c("Year", "Date", "ARI", "# Resp. symptoms", "# Systemic symptoms", "Fever", "Chills", "Headache", "Myalgia", "Malaise", "Cough", "Sorethroat", "Runnynose", "Chestpain", "Breathing", "Symptom duration", "Diagnosis", "Comments"),
        ) %>%
        kable_styling(latex_options = "striped") %>%
        column_spec(18, width = "2cm")
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
            longtable = TRUE,
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
            longtable = TRUE,
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
            longtable = TRUE,
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
            longtable = TRUE,
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
            longtable = TRUE,
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
            longtable = TRUE,
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
        arrange(year, day) %>%
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
        mutate(ic50 = round(ic50)) %>%
        kbl(
            format = "latex",
            caption = "Covid serology results. Empty if not done for this participant.",
            booktabs = TRUE,
            label = "serology-covid",
            escape = TRUE,
            longtable = TRUE,
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
            longtable = TRUE,
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
            longtable = TRUE,
            col.names = c("Year", "Location", "Date", "Brand", "Batch")
        )
})

yearly_changes_tables <- create_tables(yearly_changes, function(pid_data) {
    pid_data %>%
        arrange(redcap_project_year) %>%
        select(redcap_project_year, children, clin_care, emp_status, occupation, dept, condition) %>%
        mutate(
            condition = replace_na(condition, ""),
        ) %>%
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
            Smoker = S. Department key:
            Emergency Department = ED,
            Critical Care or Intensive Care Unit = ICU,
            General Medicine and/or Medical Specialities = GM,
            Paediatrics and/or Paediatric Specialities = PD,
            Surgery and/or Surgical Specialties = SG,
            Gynaecology and/or Obstetrics = GN,
            Oncology and/or Haematology = ON,
            Radiology = RD,
            Outpatient clinic = OC,
            Pharmacy = PH,
            Laboratory = LB,
            Nutrition = NT,
            Social Work = SW,
            Physiotherapy = PT,
            Occupational therapy = OT,
            Other = O",
            booktabs = TRUE,
            label = "yearlychanges",
            escape = TRUE,
            col.names = c("Year", "Children", "Clinical care", "Employment", "Occupation", "Department", "Condition")
        ) %>%
        kable_styling(latex_options = "scale_down")
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

vax_titres_plots <- serology %>%
    filter(pid %in% pids_to_run) %>%
    filter(vax_inf == "V", virus_vaccine) %>%
    select(-vax_inf, -virus_vaccine) %>%
    mutate(
        timepoint = factor(year * 1000 + day),
    ) %>%
    group_by(pid) %>%
    group_map(.keep = TRUE, function(pid_data, key) {
        plot <- pid_data %>%
            rename(Subtype = subtype) %>%
            ggplot(aes(timepoint, titre, color = Subtype, shape = Subtype, lty = Subtype)) + 
            theme_bw() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "bottom",
                panel.grid.minor = element_blank(),
                panel.spacing = unit(0, "null"),
                strip.background = element_blank(),
                axis.title.x = element_blank(),
            ) +
            scale_y_log10("Titre", breaks = 5 * 2^(0:20)) +
            scale_x_discrete("Timepoint", labels = function(breaks) {
                years <- str_sub(breaks, 1, 4)
                days <- str_sub(breaks, 5, 7) %>% as.integer() %>% as.character()
                paste0(years, " d", days)
            }) +
            scale_colour_viridis_d(end = 0.8) +
            coord_cartesian(
                ylim = c(min(pid_data$titre, 5), max(pid_data$titre, 1280)),
                xlim = c(min(as.integer(fct_drop(pid_data$timepoint))), max(as.integer(fct_drop(pid_data$timepoint)))),
            ) +
            facet_wrap(~virus_egg_cell, ncol = 1, strip.position = "right", labeller = as_labeller(str_to_title)) +
            geom_line(aes(group = paste0(pid, Subtype))) +
            geom_point()
        attr(plot, "key") <- key
        plot
    })

walk(vax_titres_plots, function(plot) {
    key <- attr(plot, "key")
    filename <- paste0("subject-data/reports/", key$pid, "_vaxtitres.pdf")
    ggsave(filename, plot, width = 10, height = 10, units = "cm")
})

vax_titres_dates_plot <- serology %>%
    filter(pid %in% pids_to_run) %>%
    filter(vax_inf == "V", virus_vaccine) %>%
    select(-virus_vaccine) %>%
    inner_join(bleed_dates %>% select(pid, year, day, date) %>% distinct(), c("pid", "year", "day")) %>%
    bind_rows(
        serology %>%
            filter(pid %in% pids_to_run) %>%
            filter(vax_inf == "I", virus_vaccine) %>%
            select(-virus_vaccine) %>%
            inner_join(postinf_bleed_dates %>% filter(!is.na(bleed_date)) %>% select(pid, year, day, date = bleed_date) %>% distinct(), c("pid", "year", "day")),
    ) %>%
    group_by(pid) %>%
    group_map(.keep = TRUE, function(pid_data, key) {
        plot <- pid_data %>%
            rename(Subtype = subtype) %>%
            ggplot(aes(date, titre, color = Subtype, shape = Subtype, lty = Subtype)) + 
            theme_bw() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "bottom",
                panel.grid.minor = element_blank(),
                panel.spacing = unit(0, "null"),
                strip.background = element_blank(),
                axis.title.x = element_blank(),
            ) +
            scale_y_log10("Titre", breaks = 5 * 2^(0:20)) +
            scale_x_date(breaks = "2 month") +
            scale_colour_viridis_d(end = 0.8) +
            coord_cartesian(ylim = c(min(pid_data$titre, 5), max(pid_data$titre, 1280))) +
            facet_wrap(~virus_egg_cell, ncol = 1, strip.position = "right", labeller = as_labeller(str_to_title)) +
            geom_vline(
                aes(xintercept = date),
                pid_data %>% filter(vax_inf == "I"),
                color = "gray50", alpha = 0.5
            ) +
            geom_line(aes(group = paste0(pid, Subtype))) +
            geom_point()
        attr(plot, "key") <- key
        plot
    })

walk(vax_titres_dates_plot, function(plot) {
    key <- attr(plot, "key")
    filename <- paste0("subject-data/reports/", key$pid, "_vaxtitres_dates.pdf")
    ggsave(filename, plot, width = 10, height = 10, units = "cm")
})

landscape_plots <- serology_landscapes %>%
    filter(pid %in% pids_to_run) %>%
    group_by(pid) %>%
    group_map(.keep = TRUE, function(pid_data, key) {
        plot <- pid_data %>%
            mutate(Day = factor(day)) %>%
            ggplot(aes(virus, titre, color = Day, shape = Day, lty = Day)) + 
            theme_bw() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "bottom",
                panel.grid.minor = element_blank(),
                panel.spacing = unit(0, "null"),
                strip.background = element_blank(),
                axis.title.x = element_blank(),
                plot.margin = margin(1, 1, 1, 20),
            ) +
            scale_y_log10("Titre", breaks = 5 * 2^(0:20)) +
            scale_colour_viridis_d(end = 0.8) +
            coord_cartesian(ylim = c(min(pid_data$titre, 5), max(pid_data$titre, 1280))) +
            geom_line(aes(group = paste0(pid, day))) +
            geom_point()
        attr(plot, "key") <- key
        plot
    })

walk(landscape_plots, function(plot) {
    key <- attr(plot, "key")
    filename <- paste0("subject-data/reports/", key$pid, "_landscape.pdf")
    ggsave(filename, plot, width = 20, height = 10, units = "cm")
})

ggsave("subject-data/reports/VAXTITRES.pdf", ggplot(), width = 0.5, height = 1)
ggsave("subject-data/reports/VAXTITRESDATES.pdf", ggplot(), width = 0.5, height = 1)
ggsave("subject-data/reports/LANDSCAPE.pdf", ggplot(), width = 1, height = 0.5)

template <- read_file("subject-data/reports/template.tex")

# TODO(sen) Mark vaccine strain on landscape

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

        if (key$pid %in% serology$pid) {
            pid_doc <- pid_doc %>%
                stringi::stri_replace_all_fixed("VAXTITRESDATES", paste0(key$pid, "_vaxtitres_dates")) %>%
                stringi::stri_replace_all_fixed("VAXTITRES", paste0(key$pid, "_vaxtitres"))
        }

        if (key$pid %in% serology_landscapes$pid) {
            pid_doc <- pid_doc %>%
                stringi::stri_replace_all_fixed("LANDSCAPE", paste0(key$pid, "_landscape"))
        }

        filename <- paste0("subject-data/reports/", pid_data$pid, ".tex")
        write_file(pid_doc, filename)
        cmd <- paste0("latexmk -pdf -outdir=subject-data/reports -silent ", filename)
        system(cmd, ignore.stdout = FALSE, ignore.stderr = FALSE)
    })

all_files <- list.files("subject-data/reports", full.names = TRUE)
file.remove(all_files[str_ends(all_files, "(aux)|(fls)|(fdb_latexmk)|(log)")])

system(paste0("evince ", paste0(tools::list_files_with_exts("subject-data/reports", "pdf") %>%
    basename() %>%
    `[`(str_detect(., "-") & !str_detect(., "_")) %>%
    paste0("subject-data/reports/", .) %>%
    sample(5),
    collapse = " "
)))
