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
    )
) %>%
    mutate(
        weeks_from_start = (week_start - min(week_start)) / lubridate::dweeks(1),
    )

weekly_survey_start_dates %>%
    filter(year == 2022) %>%
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
        ari_start = week_start[weeks_from_start == first_week_from_start]
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

swab_dates <- swabs %>%
    select(pid, year, swab_date = samp_date) %>%
    distinct()

find_nearest_swab_date_after <- function(pid_, year_, ari_date_) {
    all_dates_after <- swab_dates %>%
        filter(pid == pid_, year == year_, swab_date >= ari_date_) %>%
        pull(swab_date)

    if (length(all_dates_after) == 0) {
        NA
    } else {
        min(all_dates_after)
    }
}

durations_with_swab_dates <- all_durations %>%
    group_by(pid, year, ari_start) %>%
    mutate(swab_date = find_nearest_swab_date_after(pid, year, ari_start))

swab_results_collapsed <- swabs %>%
    filter(swab_result == 1) %>%
    group_by(pid, year, samp_date) %>%
    summarise(.groups = "drop", swab_result = paste0(unique(swab_virus), collapse = ";"))

durations_with_swab_results <- durations_with_swab_dates %>%
    left_join(swab_results_collapsed, c("pid", "year", "swab_date" = "samp_date"))

write_csv(durations_with_swab_results, "symptom-duration/aris.csv")
