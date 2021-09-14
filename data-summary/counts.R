library(tidyverse)

participants <- read_csv("data/participants.csv", col_types = cols())

participants_with_age_at_screening_cat <- participants %>%
  mutate(age_at_screening_cat = cut(age_screening, c(-Inf, 18, 30, 40, 50, 61, Inf), right = FALSE))

bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())

at_least_one_bleed_in_2021 <- bleed_dates %>%
  filter(year == 2021) %>%
  group_by(pid) %>%
  filter(sum(!is.na(date)) != 0) %>%
  pull(pid) %>%
  unique()

participants_currently_enrolled <- participants_with_age_at_screening_cat %>%
  filter(pid %in% at_least_one_bleed_in_2021)

calc_counts <- function(data) {
  data %>%
    group_by(site, age_at_screening_cat) %>%
    summarise(.groups = "drop", count = n(), gender = "overall") %>%
    arrange(age_at_screening_cat) %>%
    bind_rows(
      data %>%
        group_by(age_at_screening_cat) %>%
        summarise(.groups = "drop", count = n(), gender = "overall", site = "overall")
    ) %>%
    bind_rows(
      data %>%
        group_by(gender) %>%
        summarise(.groups = "drop", count = n(), age_at_screening_cat = "overall", site = "overall")
    ) %>%
    bind_rows(
      data %>%
        group_by(site, gender) %>%
        summarise(.groups = "drop", count = n(), age_at_screening_cat = "overall")
    ) %>%
    bind_rows(
      data %>%
        group_by(site) %>%
        summarise(count = n(), gender = "overall", age_at_screening_cat = "overall")
    ) %>%
    bind_rows(
      data %>%
        summarise(count = n(), site = "overall", gender = "overall", age_at_screening_cat = "overall")
    )
}

participants_currently_enrolled_counts_long <-
  calc_counts(participants_currently_enrolled)

participants_currently_enrolled_recruited_in_2020_counts_long <-
  calc_counts(
    participants_currently_enrolled %>% filter(recruitment_year == 2020)
  )

participants_currently_enrolled_counts_wide <-
  participants_currently_enrolled_counts_long %>%
  pivot_wider(names_from = "site", values_from = "count")

participants_currently_enrolled_recruited_in_2020_counts_wide <-
  participants_currently_enrolled_recruited_in_2020_counts_long %>%
  pivot_wider(names_from = "site", values_from = "count")

write_csv(
  participants_currently_enrolled_counts_wide,
  "data-summary/counts-currently-enrolled.csv"
)

write_csv(
  participants_currently_enrolled_recruited_in_2020_counts_wide,
  "data-summary/counts-currently-enrolled-recruited-in-2020.csv"
)
