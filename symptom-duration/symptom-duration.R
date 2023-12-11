library(tidyverse)

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
    ),
    tibble(
        year = 2023,
        survey_index = 1:52,
        week_start = seq(lubridate::ymd("2023-01-02"), by = 7, length.out = 52)
    )
) %>%
    mutate(
        weeks_from_start = (week_start - min(week_start)) / lubridate::dweeks(1),
    )

weekly_survey_start_dates %>%
    filter(year == 2023) %>%
    print(n = 100)

weekly_surveys <- read_csv("data/weekly-surveys.csv", col_types = cols())
daily_surveys <- read_csv("data/daily-surveys.csv", col_types = cols())
swabs <- read_csv("data/swabs.csv", col_types = cols())

weekly_surveys_ari_date_plot <- weekly_surveys %>%
    filter(ari == 1) %>%
    ggplot(aes(date, pid)) +
    theme_bw() +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
    ) +
    scale_x_date(breaks = "1 month") +
    geom_point(shape = 16, size = 0.1)

ggsave("symptom-duration/ari-date-plot.pdf", weekly_surveys_ari_date_plot, width = 15, height = 10, units = "cm")

weekly_surveys_daily_followup_symptom_duration <- daily_surveys %>%
    group_by(pid, year, weekly_survey_index) %>%
    summarise(.groups = "drop", days_symptoms_present = sum(symp_present))

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
label_consecutive(c(1, 2, 3, 4, 10, 20, 21, 22))
label_consecutive(c(1, 2, 3, 4, 10, 20, 21, 30))

weekly_surveys %>%
    inner_join(weekly_survey_start_dates, c("year", "survey_index")) %>%
    left_join(
        weekly_surveys_daily_followup_symptom_duration,
        c("pid", "year", "survey_index" = "weekly_survey_index")
    ) %>%
    filter(pid == "ALF-008", year == 2022, weeks_from_start >= 121)

all_durations <- weekly_surveys %>%
    filter(ari == 1) %>%
    inner_join(weekly_survey_start_dates, c("year", "survey_index")) %>%
    left_join(
        weekly_surveys_daily_followup_symptom_duration,
        c("pid", "year", "survey_index" = "weekly_survey_index")
    ) %>%
    group_by(pid, year) %>%
    mutate(
        consecutive_ari_group = label_consecutive(weeks_from_start)
    )  %>%
    group_by(pid, year, consecutive_ari_group) %>%
    summarise(
        .groups = "drop",
        week_span = n(),
        first_week_from_start = min(weeks_from_start),
        last_week_from_start = max(weeks_from_start),
        earliest_weekly_duration = symptom_duration[weeks_from_start == first_week_from_start],
        earliest_weekly_known_duration = if_else(
            is.na(earliest_weekly_duration) | earliest_weekly_duration >= 30, 1, earliest_weekly_duration
        ),
        last_daily_duration = days_symptoms_present[weeks_from_start == last_week_from_start],
        last_daily_known_duration = if_else(is.na(last_daily_duration), 0, last_daily_duration),
        total_known_symptom_duration =
            earliest_weekly_known_duration +
            7 * (week_span - 1) +
            last_daily_known_duration,
        ari_start = week_start[weeks_from_start == first_week_from_start],
        across(
            c(
                fever,
                chills,
                headache,
                myalgia,
                malaise,
                cough,
                sorethroat,
                runnynose,
                chestpain,
                breathing,
            ),
            ~any(.x != 0)
        )
    )

# NOTE(sen) Should be empty
all_durations %>%
    group_by(pid, year, consecutive_ari_group) %>%
    filter(n() > 1)

all_durations %>%
    arrange(desc(earliest_weekly_duration)) %>%
    print(n = 50)

all_durations %>%
    filter(total_known_symptom_duration > 30) %>%
    arrange(desc(total_known_symptom_duration))

all_durations %>%
    ggplot(aes(total_known_symptom_duration)) +
    geom_histogram(binwidth = 1)

swab_results_collapsed <- swabs %>%
    filter(swab_result == 1) %>%
    # NOTE(sen) Add rsv
    bind_rows(
        read_csv("data/swabs_other.csv", col_types = cols()) %>%
            filter(rsv) %>%
            mutate(swab_virus = "RSV", swab_result = 1) %>%
            left_join(
                swabs %>%
                    select(pid, year, postinf_instance, samp_date, site_rec_date, site_test_date, redcap_event_name) %>%
                    distinct(),
                c("pid", "year", "postinf_instance"),
            ) %>%
            select(-swab_other, -rsv)
    ) %>%
    group_by(pid, year, samp_date) %>%
    summarise(.groups = "drop", swab_result = paste0(unique(swab_virus), collapse = ";")) %>%
    mutate(swab_result_cat = case_when(
        str_detect(swab_result, "Flu A") ~ "Flu A",
        str_detect(swab_result, "Flu B") ~ "Flu B",
        str_detect(swab_result, "SARS-CoV-2") ~ "SARS-CoV-2",
        str_detect(swab_result, "RSV") ~ "RSV",
        is.na(swab_result) ~ "(missing)",
        TRUE ~ "other",
    ) %>% factor(c("Flu A", "Flu B", "SARS-CoV-2", "RSV", "other", "(missing)")))

durations_with_all_matches <- all_durations %>%
    mutate(ari_id = row_number()) %>%
    left_join(swab_results_collapsed, c("pid", "year"), relationship = "many-to-many") %>%
    filter((samp_date >= ari_start & samp_date <= ari_start + total_known_symptom_duration + 7) | is.na(samp_date)) %>%
    mutate(swab_result_cat = replace_na(swab_result_cat, "(missing)"))

durations_with_swab_results <- durations_with_all_matches %>%
    filter(.by = ari_id, samp_date == min(samp_date) | all(is.na(samp_date))) %>%
    filter(.by = ari_id, row_number() == 1)

# NOTE(sen) Check there is one row per ARI event
stopifnot((durations_with_swab_results %>% filter(.by = c(pid, year, ari_start), n() > 1) %>% nrow()) == 0)

write_csv(durations_with_swab_results, "symptom-duration/aris.csv")

durations_too_long <- durations_with_swab_results %>%
    filter(total_known_symptom_duration > 14)

swabs_with_most_closely_matching_survey <- swab_results_collapsed %>%
    left_join(
        weekly_surveys %>%
            select(
                pid, year, survey_date = date, fever, chills, headache, myalgia,
                malaise, cough, sorethroat, runnynose, chestpain, breathing
            ),
        c("pid", "year"), relationship = "many-to-many"
    ) %>%
    mutate(samp_to_survey = abs(samp_date - (survey_date - 7))) %>%
    group_by(pid, year, samp_date) %>%
    filter(samp_to_survey == min(samp_to_survey)) %>%
    filter(row_number() == 1) %>%
    filter(samp_to_survey <= 7) %>%
    ungroup() %>%
    mutate(across(c(fever, chills, headache, myalgia, malaise, cough, sorethroat, runnynose, chestpain, breathing), ~if_else(replace_na(.x, 0) == 0, 0, 1)))

swab_results_collapsed %>%
    count(swab_result) %>%
    print(n = 100)

swabs_with_matching_ari_event <- swab_results_collapsed %>%
    left_join(
        all_durations %>%
            select(
                pid, year, ari_start, total_known_symptom_duration, fever,
                chills, headache, myalgia, malaise, cough, sorethroat, runnynose,
                chestpain, breathing
            ),
        c("pid", "year"),
        relationship = "many-to-many",
    ) %>%
    filter(between(samp_date, ari_start, ari_start + total_known_symptom_duration)) %>%
    mutate(across(c(fever, chills, headache, myalgia, malaise, cough, sorethroat, runnynose, chestpain, breathing), ~replace_na(as.numeric(.x), 0)))

iwalk(
    list(
        swabs_with_most_closely_matching_survey = swabs_with_most_closely_matching_survey,
        swabs_with_matching_ari_event = swabs_with_matching_ari_event
    ),
    function(data, name) {
        data %>%
            filter(swab_result_cat != "other") %>%
            group_by(swab_result_cat) %>%
            group_walk(function(data, key) {
                plot <- data %>%
                    as.data.frame() %>%
                    UpSetR::upset(nsets = 10, nintersects = NA)
                pdf(paste0("symptom-duration/", name, "_", key$swab_result_cat, ".pdf"), onefile = FALSE, width = 10, height = 5)
                print(plot)
                dev.off()
            })
    }
)

swabs_with_matching_ari_event %>%
    filter(.by = c(pid, ari_start), n() > 1)

weekly_surveys %>%
    filter(pid %in% durations_too_long$pid) %>%
    mutate(symptom_count = n_respiratory + n_systemic) %>%
    select(pid, year, survey_index, symptom_count) %>%
    left_join(weekly_survey_start_dates, c("year", "survey_index")) %>%
    mutate(.by = pid, completed_surveys = n()) %>%
    mutate(pid = fct_reorder(pid, completed_surveys)) %>%
    (function(data) {
        pids_with_multiple_swabs_per_event <- swabs_with_matching_ari_event %>%
            filter(.by = c(pid, ari_start), n() > 1) %>%
            pull(pid)
        create_plot <- function(data) {
            data %>%
                filter(symptom_count > 0) %>%
                ggplot(aes(week_start, pid)) +
                theme_bw() +
                theme(
                    axis.text.y = element_blank(),
                    axis.title = element_blank(),
                    axis.ticks.y = element_blank(),
                    legend.position = "bottom",
                    panel.grid.minor = element_blank(),
                    panel.grid.major.y = element_blank(),
                ) +
                scale_color_continuous("Symptom count", breaks = 1:10) +
                scale_shape_manual("Swab result", values = c(24, 25, 4, 3, 16, 17), drop = FALSE) +
                geom_segment(
                    aes(xend = week_start + 7, yend = pid),
                    data = data %>% filter(symptom_count == 0), color = "gray"
                ) +
                geom_segment(aes(xend = week_start + 7, yend = pid, color = symptom_count)) +
                geom_point(
                    aes(shape = swab_result_cat),
                    data = swab_results_collapsed %>%
                        filter(pid %in% durations_too_long$pid, pid %in% pids_with_multiple_swabs_per_event) %>%
                        select(pid, week_start = samp_date, swab_result_cat) %>%
                        filter(!is.na(week_start)),
                    size = 1,
                    color = "red"
                )
        }
        data %>%
            filter(pid %in% pids_with_multiple_swabs_per_event) %>%
            create_plot() %>%
            ggsave("symptom-duration/aris_with_multiple_swabs.pdf", ., width = 20, height = 15, units = "cm")
        data %>%
            create_plot() %>%
            ggsave("symptom-duration/all_surveys.pdf", ., width = 30, height = 20, units = "cm")
    })

durations_with_swab_results %>%
    mutate(
        swab_is_found = !is.na(swab_result),
        year = as.character(year),
    ) %>%
    (function(data) {
        summarise_data <- function(durations, swab_is_found) {
            tibble(
                ari_events = length(durations), 
                mean_duration = mean(durations),
                median_duration = median(durations),
            )
        }
        data %>%
            summarise(.by = c(year, swab_result_cat), summarise_data(total_known_symptom_duration, swab_is_found)) %>%
            bind_rows(summarise(data, .by = year, summarise_data(total_known_symptom_duration, swab_is_found), swab_result_cat = "Total")) %>%
            bind_rows(summarise(data, .by = swab_result_cat, summarise_data(total_known_symptom_duration, swab_is_found), year = "Total")) %>%
            bind_rows(summarise(data, summarise_data(total_known_symptom_duration, swab_is_found), swab_result_cat = "Total", year = "Total")) %>%
            mutate(swab_result_cat = factor(swab_result_cat, c("Flu A", "Flu B", "SARS-CoV-2", "RSV", "other", "(missing)", "Total"))) %>%
            arrange(year, swab_result_cat) %>%
            print(n = 100) %>%
            write_csv("symptom-duration/ari_events_by_year.csv")
    })

durations_with_swab_results %>%
    count(swab_result) %>%
    print(n = 1000)

