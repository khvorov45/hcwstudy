library(tidyverse)

infections_from_swabs <- read_csv("data/swabs.csv", col_types = cols()) %>%
    filter(swab_result == 1, swab_virus == "SARS-CoV-2" | str_starts(swab_virus, "Flu")) %>%
    select(pid, year, samp_date, swab_virus) %>%
    distinct()

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

infections_from_surveys <- read_csv("data/weekly-surveys.csv", col_types = cols()) %>%
    filter(diagnosis %in% c("flu", "covid")) %>%
    left_join(
        infections_from_swabs %>%
            mutate(diagnosis = if_else(swab_virus == "SARS-CoV-2", "covid", "flu")) %>%
            select(-swab_virus) %>%
            distinct(),
        c("pid", "year", "diagnosis"),
        relationship = "many-to-many",
    ) %>%
    group_by(pid, year, date, diagnosis) %>%
    # NOTE(sen) This cutoff for "close enough" is arbitrary
    mutate(swab_is_close_enough = any(date - samp_date <= 22) %>% replace_na(FALSE)) %>%
    filter(!swab_is_close_enough) %>%
    ungroup() %>%
    select(pid, year, survey_index, date, diagnosis, comments) %>%
    distinct() %>%
    arrange(pid, year, date, diagnosis) %>%
    mutate(.by = c(pid, year, diagnosis), con = label_consecutive(survey_index)) %>%
    summarise(.by = c(pid, year, diagnosis, con), date = min(date), comments = paste0(comments, collapse = "\n===\n")) %>%
    select(-con) %>%
    # NOTE(sen) Look through the comments manually
    write_csv("temp.csv") %>%
    (function(data) {
        data %>%
            mutate(
                diagnosis = if_else(diagnosis == "flu", paste0("  \"", diagnosis, "\""), paste0("\"", diagnosis, "\"")),
                str = glue::glue("#pid == \"{pid}\" & year == {year} & diagnosis == {diagnosis} & date == ymd(\"{date}\") ~ ymd(\"{date}\"),")
            ) %>%
            pull(str) %>%
            paste0(collapse = "\n") %>%
            write("temp.txt")
        data
    }) %>%
    mutate(date = case_when(
        pid == "ALF-007" & year == 2023 & diagnosis == "covid" & date == ymd("2023-08-03") ~ ymd("2023-07-30"),
        pid == "ALF-028" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-22") ~ ymd("2023-05-20"),
        pid == "ALF-039" & year == 2023 & diagnosis == "covid" & date == ymd("2023-10-23") ~ ymd("2023-10-21"),
        pid == "ALF-059" & year == 2023 & diagnosis == "covid" & date == ymd("2023-10-23") ~ ymd("2023-10-20"),
        #pid == "ALF-064" & year == 2021 & diagnosis == "covid" & date == ymd("2021-12-06") ~ ymd("2021-12-06"),
        #pid == "ALF-071" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-06-24") ~ ymd("2021-06-24"),
        #pid == "ALF-087" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-21") ~ ymd("2022-07-21"),
        #pid == "ALF-096" & year == 2023 & diagnosis == "covid" & date == ymd("2023-07-03") ~ ymd("2023-07-03"),
        pid == "ALF-099" & year == 2023 & diagnosis == "covid" & date == ymd("2023-10-09") ~ ymd("2023-10-04"),
        #pid == "ALF-102" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-06") ~ ymd("2022-06-06"),
        #pid == "ALF-110" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-25") ~ ymd("2022-07-25"),
        #pid == "ALF-124" & year == 2021 & diagnosis == "covid" & date == ymd("2022-01-03") ~ ymd("2022-01-03"),
        pid == "ALF-131" & year == 2023 & diagnosis == "covid" & date == ymd("2023-10-24") ~ ymd("2023-10-22"),
        pid == "ALF-135" & year == 2023 & diagnosis == "covid" & date == ymd("2023-10-23") ~ ymd("2023-10-17"),
        pid == "ALF-157" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-01") ~ ymd("2023-04-21"),
        pid == "ALF-159" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-18") ~ ymd("2023-09-15"),
        #pid == "ALF-161" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-08-14") ~ ymd("2023-08-14"),
        #pid == "ALF-164" & year == 2023 & diagnosis == "covid" & date == ymd("2023-10-02") ~ ymd("2023-10-02"),
        #pid == "ALF-169" & year == 2023 & diagnosis == "covid" & date == ymd("2023-07-24") ~ ymd("2023-07-24"),
        #pid == "ALF-802" & year == 2022 & diagnosis == "covid" & date == ymd("2022-05-26") ~ ymd("2022-05-26"),
        #pid == "ALF-806" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-12") ~ ymd("2023-06-12"),
        #pid == "ALF-807" & year == 2022 & diagnosis == "covid" & date == ymd("2022-03-28") ~ ymd("2022-03-28"),
        #pid == "ALF-819" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-11") ~ ymd("2023-09-11"),
        #pid == "CHW-001" & year == 2022 & diagnosis == "covid" & date == ymd("2022-05-20") ~ ymd("2022-05-20"),
        #pid == "CHW-001" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-15") ~ ymd("2022-06-15"),
        #pid == "CHW-030" & year == 2022 & diagnosis == "covid" & date == ymd("2022-03-21") ~ ymd("2022-03-21"),
        #pid == "CHW-031" & year == 2022 & diagnosis == "covid" & date == ymd("2022-04-25") ~ ymd("2022-04-25"),
        #pid == "CHW-035" & year == 2021 & diagnosis == "covid" & date == ymd("2022-01-11") ~ ymd("2022-01-11"),
        #pid == "CHW-047" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-18") ~ ymd("2022-08-18"),
        #pid == "CHW-048" & year == 2020 & diagnosis ==   "flu" & date == ymd("2020-05-25") ~ ymd("2020-05-25"),
        #pid == "CHW-048" & year == 2020 & diagnosis ==   "flu" & date == ymd("2020-11-09") ~ ymd("2020-11-09"),
        #pid == "CHW-048" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-02-10") ~ ymd("2022-02-10"),
        #pid == "CHW-048" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-06-08") ~ ymd("2023-06-08"),
        #pid == "CHW-052" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-05") ~ ymd("2023-06-05"),
        #pid == "CHW-058" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-09-26") ~ ymd("2022-09-26"),
        #pid == "CHW-071" & year == 2020 & diagnosis ==   "flu" & date == ymd("2020-07-29") ~ ymd("2020-07-29"),
        #pid == "CHW-072" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-06") ~ ymd("2022-06-06"),
        #pid == "CHW-079" & year == 2023 & diagnosis == "covid" & date == ymd("2023-10-23") ~ ymd("2023-10-23"),
        #pid == "CHW-098" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-06-07") ~ ymd("2021-06-07"),
        #pid == "CHW-098" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-06-07") ~ ymd("2022-06-07"),
        #pid == "CHW-107" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-04") ~ ymd("2023-09-04"),
        #pid == "CHW-118" & year == 2022 & diagnosis == "covid" & date == ymd("2022-09-12") ~ ymd("2022-09-12"),
        #pid == "CHW-124" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-05-30") ~ ymd("2022-05-30"),
        #pid == "CHW-124" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-15") ~ ymd("2023-05-15"),
        #pid == "CHW-130" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-10-02") ~ ymd("2023-10-02"),
        #pid == "CHW-133" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-05-09") ~ ymd("2022-05-09"),
        pid == "CHW-133" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-01") ~ ymd("2022-07-26"),
        #pid == "CHW-143" & year == 2022 & diagnosis == "covid" & date == ymd("2022-09-06") ~ ymd("2022-09-06"),
        pid == "CHW-157" & year == 2023 & diagnosis == "covid" & date == ymd("2023-10-23") ~ ymd("2023-10-18"),
        #pid == "CHW-164" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-10-17") ~ ymd("2022-10-17"),
        #pid == "CHW-164" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-11") ~ ymd("2023-09-11"),
        pid == "CHW-170" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-19") ~ ymd("2023-06-15"),
        pid == "CHW-174" & year == 2022 & diagnosis == "covid" & date == ymd("2022-04-04") ~ ymd("2022-03-30"),
        #pid == "CHW-202" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-07-24") ~ ymd("2023-07-24"),
        #pid == "CHW-224" & year == 2022 & diagnosis == "covid" & date == ymd("2022-05-18") ~ ymd("2022-05-18"),
        #pid == "CHW-247" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-07-24") ~ ymd("2023-07-24"),
        #pid == "CHW-247" & year == 2023 & diagnosis == "covid" & date == ymd("2023-08-03") ~ ymd("2023-08-03"),
        #pid == "CHW-253" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-03-28") ~ ymd("2022-03-28"),
        #pid == "CHW-274" & year == 2023 & diagnosis == "covid" & date == ymd("2023-07-17") ~ ymd("2023-07-17"),
        #pid == "CHW-289" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-29") ~ ymd("2023-05-29"),
        #pid == "CHW-300" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-08") ~ ymd("2023-05-08"),
        #pid == "CHW-303" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-05") ~ ymd("2023-06-05"),
        #pid == "CHW-319" & year == 2023 & diagnosis == "covid" & date == ymd("2023-07-21") ~ ymd("2023-07-21"),
        pid == "CHW-330" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-24") ~ ymd("2023-05-19"),
        #pid == "CHW-338" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-10-23") ~ ymd("2023-10-23"),
        pid == "CHW-802" & year == 2023 & diagnosis == "covid" & date == ymd("2023-08-18") ~ ymd("2023-08-17"),
        #pid == "JHH-009" & year == 2020 & diagnosis ==   "flu" & date == ymd("2020-09-21") ~ ymd("2020-09-21"),
        #pid == "JHH-029" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-06-07") ~ ymd("2021-06-07"),
        #pid == "JHH-050" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-30") ~ ymd("2022-06-30"),
        #pid == "JHH-096" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-03-28") ~ ymd("2022-03-28"),
        pid == "JHH-096" & year == 2022 & diagnosis == "covid" & date == ymd("2022-09-07") ~ ymd("2022-09-03"),
        pid == "JHH-117" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-25") ~ ymd("2022-07-19"),
        #pid == "JHH-149" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-05-17") ~ ymd("2021-05-17"),
        #pid == "JHH-170" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-19") ~ ymd("2022-07-19"),
        pid == "JHH-172" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-18") ~ ymd("2022-07-15"),
        pid == "JHH-210" & year == 2022 & diagnosis == "covid" & date == ymd("2022-05-02") ~ ymd("2022-04-21"),
        pid == "JHH-216" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-16") ~ ymd("2023-06-10"),
        pid == "JHH-225" & year == 2021 & diagnosis == "covid" & date == ymd("2021-12-20") ~ ymd("2021-12-18"),
        pid == "JHH-225" & year == 2022 & diagnosis == "covid" & date == ymd("2022-09-05") ~ ymd("2022-09-01"),
        #pid == "JHH-245" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-22") ~ ymd("2023-05-22"),
        #pid == "JHH-245" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-06-05") ~ ymd("2023-06-05"),
        #pid == "JHH-257" & year == 2021 & diagnosis == "covid" & date == ymd("2022-01-03") ~ ymd("2022-01-03"),
        #pid == "JHH-284" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-07-24") ~ ymd("2023-07-24"),
        #pid == "JHH-298" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-04-11") ~ ymd("2022-04-11"),
        #pid == "JHH-298" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-05-23") ~ ymd("2022-05-23"),
        pid == "JHH-303" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-29") ~ ymd("2022-08-09"),
        #pid == "JHH-306" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-16") ~ ymd("2022-06-16"),
        #pid == "JHH-328" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-05-30") ~ ymd("2022-05-30"),
        #pid == "JHH-362" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-04") ~ ymd("2022-07-04"),
        #pid == "JHH-364" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-04-17") ~ ymd("2023-04-17"),
        pid == "JHH-370" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-19") ~ ymd("2022-07-17"),
        pid == "JHH-435" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-19") ~ ymd("2023-06-09"),
        #pid == "JHH-450" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-08-14") ~ ymd("2023-08-14"),
        #pid == "JHH-469" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-29") ~ ymd("2023-05-29"),
        #pid == "JHH-485" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-05-29") ~ ymd("2023-05-29"),
        #pid == "JHH-488" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-04") ~ ymd("2023-09-04"),
        #pid == "JHH-495" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-19") ~ ymd("2023-06-19"),
        #pid == "JHH-495" & year == 2023 & diagnosis == "covid" & date == ymd("2023-07-11") ~ ymd("2023-07-11"),
        #pid == "JHH-506" & year == 2023 & diagnosis == "covid" & date == ymd("2023-10-25") ~ ymd("2023-10-25"),
        #pid == "JHH-541" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-16") ~ ymd("2023-05-16"),
        #pid == "JHH-802" & year == 2020 & diagnosis ==   "flu" & date == ymd("2020-06-15") ~ ymd("2020-06-15"),
        #pid == "JHH-811" & year == 2022 & diagnosis == "covid" & date == ymd("2022-02-10") ~ ymd("2022-02-10"),
        #pid == "JHH-818" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-05") ~ ymd("2023-06-05"),
        #pid == "PCH-010" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-06-07") ~ ymd("2021-06-07"),
        #pid == "PCH-029" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-26") ~ ymd("2023-06-26"),
        #pid == "PCH-097" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-18") ~ ymd("2022-07-18"),
        #pid == "PCH-103" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-12") ~ ymd("2023-06-12"),
        #pid == "PCH-104" & year == 2021 & diagnosis == "covid" & date == ymd("2021-05-25") ~ ymd("2021-05-25"),
        #pid == "PCH-132" & year == 2020 & diagnosis == "covid" & date == ymd("2020-06-15") ~ ymd("2020-06-15"),
        #pid == "PCH-133" & year == 2023 & diagnosis == "covid" & date == ymd("2023-08-14") ~ ymd("2023-08-14"),
        #pid == "PCH-154" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-06-16") ~ ymd("2021-06-16"),
        #pid == "PCH-154" & year == 2023 & diagnosis == "covid" & date == ymd("2023-04-26") ~ ymd("2023-04-26"),
        pid == "PCH-173" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-01") ~ ymd("2023-04-29"),
        pid == "PCH-189" & year == 2023 & diagnosis == "covid" & date == ymd("2023-08-14") ~ ymd("2023-08-10"),
        pid == "PCH-190" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-06") ~ ymd("2023-06-02"),
        #pid == "PCH-206" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-25") ~ ymd("2022-07-25"),
        #pid == "PCH-214" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-08") ~ ymd("2023-05-08"),
        #pid == "PCH-215" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-07-28") ~ ymd("2022-07-28"),
        pid == "PCH-221" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-08") ~ ymd("2023-08-16"),
        #pid == "PCH-223" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-09-02") ~ ymd("2021-09-02"),
        #pid == "PCH-224" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-09-02") ~ ymd("2022-09-02"),
        #pid == "PCH-242" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-08-23") ~ ymd("2021-08-23"),
        #pid == "PCH-256" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-01") ~ ymd("2023-05-01"),
        #pid == "PCH-267" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-12") ~ ymd("2022-07-12"),
        #pid == "PCH-269" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-09-12") ~ ymd("2022-09-12"),
        #pid == "PCH-281" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-08") ~ ymd("2022-06-08"),
        pid == "PCH-284" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-01") ~ ymd("2022-07-25"),
        pid == "PCH-287" & year == 2022 & diagnosis == "covid" & date == ymd("2022-05-23") ~ ymd("2022-05-10"),
        pid == "PCH-303" & year == 2023 & diagnosis == "covid" & date == ymd("2023-08-21") ~ ymd("2023-08-10"),
        #pid == "PCH-304" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-12") ~ ymd("2023-06-12"),
        #pid == "PCH-308" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-08-25") ~ ymd("2022-08-25"),
        #pid == "PCH-311" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-13") ~ ymd("2022-06-13"),
        #pid == "PCH-315" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-06") ~ ymd("2022-06-06"),
        #pid == "PCH-324" & year == 2022 & diagnosis == "covid" & date == ymd("2022-05-23") ~ ymd("2022-05-23"),
        #pid == "PCH-326" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-06") ~ ymd("2022-06-06"),
        pid == "PCH-327" & year == 2022 & diagnosis == "covid" & date == ymd("2022-09-19") ~ ymd("2022-09-15"),
        pid == "PCH-327" & year == 2022 & diagnosis == "covid" & date == ymd("2022-10-24") ~ ymd("2022-09-15"),
        pid == "PCH-331" & year == 2023 & diagnosis == "covid" & date == ymd("2023-04-20") ~ ymd("2023-04-08"),
        pid == "PCH-343" & year == 2023 & diagnosis == "covid" & date == ymd("2023-07-10") ~ ymd("2023-07-06"),
        #pid == "PCH-344" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-07") ~ ymd("2022-07-07"),
        #pid == "PCH-346" & year == 2022 & diagnosis == "covid" & date == ymd("2022-05-30") ~ ymd("2022-05-30"),
        #pid == "PCH-346" & year == 2023 & diagnosis == "covid" & date == ymd("2023-08-14") ~ ymd("2023-08-14"),
        pid == "PCH-352" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-07") ~ ymd("2022-06-30"),
        #pid == "PCH-354" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-25") ~ ymd("2022-07-25"),
        pid == "PCH-362" & year == 2022 & diagnosis == "covid" & date == ymd("2022-09-02") ~ ymd("2022-08-31"),
        #pid == "PCH-366" & year == 2022 & diagnosis == "covid" & date == ymd("2022-10-10") ~ ymd("2022-10-10"),
        #pid == "PCH-386" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-07-18") ~ ymd("2022-07-18"),
        #pid == "PCH-386" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-08-31") ~ ymd("2023-08-31"),
        #pid == "PCH-389" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-18") ~ ymd("2022-07-18"),
        #pid == "PCH-391" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-11") ~ ymd("2022-07-11"),
        #pid == "PCH-409" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-18") ~ ymd("2023-05-18"),
        #pid == "PCH-414" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-08-07") ~ ymd("2023-08-07"),
        #pid == "PCH-419" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-05") ~ ymd("2023-06-05"),
        #pid == "PCH-422" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-05-22") ~ ymd("2023-05-22"),
        #pid == "PCH-423" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-15") ~ ymd("2023-05-15"),
        pid == "PCH-439" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-29") ~ ymd("2023-05-24"),
        pid == "PCH-441" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-22") ~ ymd("2023-05-20"),
        #pid == "PCH-448" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-21") ~ ymd("2023-09-21"),
        #pid == "PCH-483" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-06-19") ~ ymd("2023-06-19"),
        #pid == "PCH-483" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-11") ~ ymd("2023-09-11"),
        #pid == "PCH-485" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-07-10") ~ ymd("2023-07-10"),
        #pid == "PCH-485" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-10-03") ~ ymd("2023-10-03"),
        #pid == "PCH-500" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-11") ~ ymd("2023-09-11"),
        #pid == "PCH-515" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-30") ~ ymd("2023-05-30"),
        #pid == "PCH-518" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-25") ~ ymd("2023-05-25"),
        pid == "PCH-522" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-12") ~ ymd("2023-06-09"),
        pid == "PCH-527" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-11") ~ ymd("2023-09-08"),
        #pid == "PCH-809" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-14") ~ ymd("2023-06-14"),
        #pid == "PCH-810" & year == 2020 & diagnosis ==   "flu" & date == ymd("2020-08-26") ~ ymd("2020-08-26"),
        pid == "PCH-829" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-29") ~ ymd("2022-08-17"),
        pid == "PCH-829" & year == 2023 & diagnosis == "covid" & date == ymd("2023-04-20") ~ ymd("2023-04-03"),
        #pid == "PCH-831" & year == 2023 & diagnosis == "covid" & date == ymd("2023-04-18") ~ ymd("2023-04-18"),
        #pid == "PCH-838" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-01") ~ ymd("2022-08-01"),
        pid == "PCH-839" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-25") ~ ymd("2022-07-15"),
        pid == "PCH-839" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-06-26") ~ ymd("2023-06-20"),
        pid == "PCH-839" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-07-03") ~ ymd("2023-06-20"),
        pid == "QCH-007" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-08") ~ ymd("2023-05-31"),
        #pid == "QCH-012" & year == 2022 & diagnosis == "covid" & date == ymd("2022-05-09") ~ ymd("2022-05-09"),
        pid == "QCH-013" & year == 2023 & diagnosis == "covid" & date == ymd("2023-07-03") ~ ymd("2023-06-23"),
        #pid == "QCH-030" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-09") ~ ymd("2022-06-09"),
        #pid == "QCH-030" & year == 2023 & diagnosis == "covid" & date == ymd("2023-10-02") ~ ymd("2023-10-02"),
        pid == "QCH-034" & year == 2023 & diagnosis == "covid" & date == ymd("2023-07-02") ~ ymd("2023-06-13"),
        #pid == "QCH-041" & year == 2022 & diagnosis == "covid" & date == ymd("2022-04-25") ~ ymd("2022-04-25"),
        pid == "QCH-052" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-09") ~ ymd("2023-06-05"),
        #pid == "QCH-057" & year == 2023 & diagnosis == "covid" & date == ymd("2023-04-17") ~ ymd("2023-04-17"),
        #pid == "QCH-063" & year == 2020 & diagnosis ==   "flu" & date == ymd("2020-06-01") ~ ymd("2020-06-01"),
        #pid == "QCH-063" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-06-16") ~ ymd("2022-06-16"),
        #pid == "QCH-072" & year == 2022 & diagnosis == "covid" & date == ymd("2022-10-26") ~ ymd("2022-10-26"),
        #pid == "QCH-073" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-28") ~ ymd("2022-06-28"),
        #pid == "QCH-074" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-09-27") ~ ymd("2021-09-27"),
        pid == "QCH-090" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-29") ~ ymd("2023-05-26"),
        #pid == "QCH-095" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-25") ~ ymd("2022-07-25"),
        #pid == "QCH-106" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-06") ~ ymd("2023-06-06"),
        #pid == "QCH-128" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-13") ~ ymd("2022-06-13"),
        pid == "QCH-129" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-22") ~ ymd("2023-05-17"),
        pid == "QCH-130" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-08") ~ ymd("2023-05-08"),
        #pid == "QCH-142" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-06-11") ~ ymd("2021-06-11"),
        #pid == "QCH-142" & year == 2021 & diagnosis ==   "flu" & date == ymd("2021-06-14") ~ ymd("2021-06-14"),
        #pid == "QCH-145" & year == 2022 & diagnosis == "covid" & date == ymd("2022-05-13") ~ ymd("2022-05-13"),
        #pid == "QCH-154" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-15") ~ ymd("2023-05-15"),
        #pid == "QCH-163" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-09-19") ~ ymd("2022-09-19"),
        #pid == "QCH-163" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-10-10") ~ ymd("2022-10-10"),
        #pid == "QCH-174" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-25") ~ ymd("2023-09-25"),
        #pid == "QCH-189" & year == 2022 & diagnosis ==   "flu" & date == ymd("2022-07-01") ~ ymd("2022-07-01"),
        #pid == "QCH-195" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-22") ~ ymd("2022-08-22"),
        #pid == "QCH-221" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-08") ~ ymd("2022-08-08"),
        #pid == "QCH-238" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-01") ~ ymd("2022-08-01"),
        pid == "QCH-242" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-22") ~ ymd("2022-08-16"),
        pid == "QCH-278" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-09") ~ ymd("2022-08-04"),
        #pid == "QCH-289" & year == 2023 & diagnosis == "covid" & date == ymd("2023-04-24") ~ ymd("2023-04-24"),
        pid == "QCH-290" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-08") ~ ymd("2023-06-02"),
        pid == "QCH-292" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-26") ~ ymd("2023-06-19"),
        #pid == "QCH-309" & year == 2023 & diagnosis == "covid" & date == ymd("2023-09-28") ~ ymd("2023-09-28"),
        #pid == "QCH-317" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-06-14") ~ ymd("2023-06-14"),
        pid == "QCH-320" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-22") ~ ymd("2023-05-19"),
        #pid == "QCH-327" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-07-31") ~ ymd("2023-07-31"),
        #pid == "QCH-803" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-22") ~ ymd("2023-05-22"),
        #pid == "QCH-817" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-11") ~ ymd("2022-07-11"),
        #pid == "QCH-821" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-30") ~ ymd("2023-05-30"),
        #pid == "QCH-831" & year == 2023 & diagnosis == "covid" & date == ymd("2023-05-05") ~ ymd("2023-05-05"),
        pid == "WCH-029" & year == 2022 & diagnosis == "covid" & date == ymd("2022-08-29") ~ ymd("2022-08-23"),
        pid == "WCH-034" & year == 2022 & diagnosis == "covid" & date == ymd("2022-04-25") ~ ymd("2022-04-19"),
        #pid == "WCH-039" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-08-17") ~ ymd("2023-08-17"),
        #pid == "WCH-039" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-08-28") ~ ymd("2023-08-28"),
        #pid == "WCH-079" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-30") ~ ymd("2022-06-30"),
        pid == "WCH-086" & year == 2023 & diagnosis == "covid" & date == ymd("2023-04-30") ~ ymd("2023-04-21"),
        pid == "WCH-088" & year == 2023 & diagnosis == "covid" & date == ymd("2023-04-17") ~ ymd("2023-04-17"),
        pid == "WCH-090" & year == 2022 & diagnosis == "covid" & date == ymd("2022-05-12") ~ ymd("2022-05-08"),
        #pid == "WCH-095" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-20") ~ ymd("2022-06-20"),
        #pid == "WCH-107" & year == 2022 & diagnosis == "covid" & date == ymd("2022-06-01") ~ ymd("2022-06-01"),
        #pid == "WCH-110" & year == 2022 & diagnosis == "covid" & date == ymd("2022-09-19") ~ ymd("2022-09-19"),
        #pid == "WCH-140" & year == 2023 & diagnosis ==   "flu" & date == ymd("2023-10-05") ~ ymd("2023-10-05"),
        #pid == "WCH-804" & year == 2023 & diagnosis == "covid" & date == ymd("2023-06-13") ~ ymd("2023-06-13"),
        #pid == "WCH-813" & year == 2022 & diagnosis == "covid" & date == ymd("2022-07-19") ~ ymd("2022-07-19"),
        TRUE ~ date,
    )) %>%
    select(-comments) %>%
    distinct()

infections_combined <- infections_from_swabs %>%
    rename(date = samp_date, diagnosis = swab_virus) %>%
    mutate(diagnosis = if_else(diagnosis == "SARS-CoV-2", "covid", "flu"), source = "swab") %>%
    bind_rows(infections_from_surveys %>% mutate(source = "survey")) %>%
    filter(.by = c(pid, year, date, diagnosis), n() == 1 | source == "swab") %>%
    distinct()

write_csv(infections_combined, "misc/infections_combined.csv")
