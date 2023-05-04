library(tidyverse)

participants <- read_csv("data/participants.csv", col_types = cols())
consent <- read_csv("data/consent.csv", col_types = cols())
swabs <- read_csv("data/swabs.csv", col_types = cols())
surveys <- read_csv("data/weekly-surveys.csv", col_types = cols())
postinf_bleed_dates <- read_csv("data/postinf-bleed-dates.csv", col_types = cols())
bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())
covid_bleed_dates <- read_csv("data/covid-bleed-dates.csv", col_types = cols())
covid_vax <- read_csv("data/covid-vax.csv", col_types = cols())
withdrawn <- read_csv("data/withdrawn.csv", col_types = cols())
serology_covid <- read_csv("data/serology-covid.csv", col_types = cols())

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

all_covid_infections <- swabs %>%
    filter(swab_virus == "SARS-CoV-2", swab_result == 1) %>%
    select(pid, year, postinf_instance, samp_date)

all_covid_infections %>%
    left_join(
        postinf_bleed_dates %>%
            summarise(.by = c(pid, year, postinf_instance), bled = any(!is.na(bleed_date)), bleed_dates = paste(sort(bleed_date), collapse = ",")),
        c("pid", "year", "postinf_instance")
    ) %>%
    select(-postinf_instance) %>%
    filter(.by = c(pid, year, samp_date), n() == 1 | !is.na(bled)) %>%
    group_by(pid) %>%
    arrange(samp_date) %>%
    mutate(
        weeks_from_start = week(samp_date) + 52 * (year(samp_date) - 2020),
        event_index = label_consecutive(weeks_from_start),
    ) %>%
    ungroup() %>%
    summarise(.by = c(pid, event_index), bled = any(!is.na(bled)), swab_date = first(samp_date), bleed_dates = first(bleed_dates)) %>%
    select(-event_index) %>%
    left_join(participants %>% select(pid, site), "pid") %>%
    select(pid, site, swab_date, bled, bleed_dates) %>%
    arrange(pid) %>%
    write_csv("leslie/covid-infection-bleed-followup.csv")

all_postvax_pbmcs <- bleed_dates %>%
    filter(samp_type == "pbmc" | samp_type == "both") %>%
    bind_rows(covid_bleed_dates %>% filter(str_detect(samp_type, "pbmc"))) %>%
    filter(day %in% c(7, 14)) %>%
    select(-day, -samp_type)

covid_vax %>%
    # NOTE(sen) Only the ones who consented to the nested study
    inner_join(
        consent %>%
            filter(disease == "covid", consent == "nested") %>%
            select("pid") %>%
            distinct(),
        "pid"
    ) %>%
    select(pid, dose, vax_date = date) %>%
    left_join(all_postvax_pbmcs %>% select(pid, postvax_pbmc_bleed_date = date), "pid", relationship = "many-to-many") %>%
    mutate(close_enough = postvax_pbmc_bleed_date - vax_date <= 7 & postvax_pbmc_bleed_date - vax_date >= 0) %>%
    summarise(.by = c(pid, dose), have_pbmc = any(close_enough)) %>%
    mutate(have_pbmc = replace_na(have_pbmc, FALSE)) %>%
    filter(have_pbmc) %>%
    select(-have_pbmc) %>%
    left_join(participants %>% select(pid, site), "pid") %>%
    select(pid, site, dose) %>%
    arrange(pid) %>%
    write_csv("leslie/covid-vax-have-pbmc.csv")

annette_list <- readxl::read_excel("leslie/Additional Followup of COVID infected.xlsx", range = "C1:D75")

annette_list %>%
    mutate(pid = recode(PID, "ALF-092" = "ALF-819", "JHH-830" = "JHH-082")) %>%
    left_join(
        read_csv("data/yearly-changes.csv", col_types = cols()) %>%
            filter(redcap_project_year == 2022) %>%
            select(pid, redcap_project_year, pid_og), c("pid" = "pid")
    ) %>%
    group_by(Site) %>%
    group_map(function(data, key) {
        paste0("[baseline_arm_1][pid]='", data$pid_og, "'", collapse = " or\n")
    }) %>%
    paste(collapse = "\n\n") %>%
    cat()

withdrawn_for_sure <- withdrawn %>%
    filter(withdrawn == 1) %>%
    filter(.by = pid, withdrawal_date == max(withdrawal_date)) %>%
    filter(withdrawn_reentered == 0)

had_2022_d220_bleed <- bleed_dates %>% filter(day == 220, year == 2022) %>% select(pid) %>% distinct()

consented_to_covid <- consent %>% filter(disease == "covid", consent != "no")

covid_bleed_dates %>%
    filter(!pid %in% withdrawn_for_sure$pid) %>%
    filter(pid %in% had_2022_d220_bleed$pid) %>%
    filter(pid %in% consented_to_covid$pid) %>%
    select(pid) %>%
    distinct()

swabs %>% filter(year == 2022, swab_result == 1, swab_virus == "SARS-CoV-2") %>% select(pid) %>% distinct()

bleed_dates %>% filter(year == 2022) %>% select(pid) %>% distinct()

serology_covid %>% filter(bleed_day_id == 14, vax_inf == "V") %>% mutate(logic50 = log(ic50)) %>% pull(ic50) %>% log() %>% hist(breaks = 50)

simone <- function(n_followups, covid_prop_lowt, titre_effect) {
    tibble(
        logtitre_postvax = rnorm(n_followups, 4, 1.5),
        infected = rbinom(n_followups, 1, covid_prop_lowt * (titre_effect ^ logtitre_postvax))
    )
}

simone(3000, 0.2, 0.7) %>%
    mutate(logtitrecat = cut(logtitre_postvax, c(-Inf, 2, 4, 6, Inf))) %>%
    summarise(.by = logtitrecat, prop = sum(infected) / n()) %>%
    arrange(logtitrecat)

glm(infected ~ logtitre_postvax, binomial, simone(3000, 0.2, 0.8)) %>% 
    broom::tidy() %>%
    filter(term == "logtitre_postvax") %>%
    mutate(low = estimate - 1.96 * std.error, high = estimate + 1.96 * std.error) %>%
    select(estimate, low, high) %>%
    pivot_longer(everything(), names_to = "kind", values_to = "value") %>%
    mutate(logprot50 = log(0.5) / value, prot50 = exp(logprot50)) %>%
    select(kind, prot50)

expand.grid(
    n_followups = c(300),
    covid_prop_lowt = c(0.2),
    titre_effect = c(0.75)
) %>%
    pmap_dfr(function(n_followups, covid_prop_lowt, titre_effect) {
        map_dfr(1:100, function(index) {
            pop <- simone(n_followups, covid_prop_lowt, titre_effect)
            fit <- glm(infected ~ logtitre_postvax, binomial, pop)
            bind_cols(broom::tidy(fit), tibble(n_followups, covid_prop_lowt, titre_effect, index))
        })    
    }) %>%
    filter(term == "logtitre_postvax") %>%
    mutate(low = estimate - 1.96 * std.error, high = estimate + 1.96 * std.error) %>%
    select(-term, -std.error, -statistic, -p.value) %>%
    pivot_longer(c(estimate, low, high), names_to = "kind", values_to = "value") %>%
    mutate(logprot50 = log(0.5) / value, prot50 = signif(exp(logprot50), 2)) %>%
    select(-value, -logprot50) %>%
    pivot_wider(names_from = kind, values_from = prot50) %>%
    summarise(.by = c(-estimate, -low, -high, -index), interval = mean(high - low))
