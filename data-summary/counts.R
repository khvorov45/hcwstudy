library(tidyverse)

participants <- read_csv("data/participants.csv", col_types = cols())

bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())

vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())

vaccinations_with_participant_info <-
  inner_join(vaccinations, participants %>% select(pid, arm, recruitment_year, site), "pid")

prior_vaccinations <- vaccinations_with_participant_info %>%
  filter(year >= recruitment_year - 5, year < recruitment_year) %>%
  group_by(pid, arm, site) %>%
  summarise(.groups = "drop", n_prior_vaccinations = sum(status %in% c("Australia", "Overseas"), na.rm = TRUE))

participants_with_extra <- participants %>%
  mutate(age_at_screening_cat = cut(age_screening, c(-Inf, 18, 30, 40, 50, 61, Inf), right = FALSE)) %>%
  left_join(prior_vaccinations %>% select(pid, n_prior_vaccinations), "pid") %>%
  # NOTE(sen) No known prior vaccinations = zero prior vaccinations
  mutate(n_prior_vaccinations = replace_na(n_prior_vaccinations, 0))

swabs <- read_csv("data/swabs.csv", col_types = cols())

# SECTION Currently enrolled

at_least_one_bleed_in_2021 <- bleed_dates %>%
  filter(year == 2021) %>%
  group_by(pid) %>%
  filter(sum(!is.na(date)) != 0) %>%
  pull(pid) %>%
  unique()

participants_currently_enrolled <- participants_with_extra %>%
  filter(pid %in% at_least_one_bleed_in_2021)

calc_counts <- function(data) {
  data <- data %>% mutate(n_prior_vaccinations = as.character(n_prior_vaccinations))
  data %>%
    # NOTE(sen) Site and prior vaccinations
    group_by(site, n_prior_vaccinations) %>%
    summarise(
      .groups = "drop", count = n(),
      gender = "overall", age_at_screening_cat = "overall"
    ) %>%
    # NOTE(sen) Site and age
    bind_rows(data %>% group_by(site, age_at_screening_cat) %>%
      summarise(
        .groups = "drop", count = n(),
        gender = "overall", n_prior_vaccinations = "overall"
      ) %>%
      arrange(age_at_screening_cat)) %>%
    # NOTE(sen) Site and gender
    bind_rows(data %>%
      group_by(site, gender) %>%
      summarise(
        .groups = "drop", count = n(),
        age_at_screening_cat = "overall", n_prior_vaccinations = "overall"
      )) %>%
    # NOTE(sen) Prior vaccinations
    bind_rows(data %>% group_by(n_prior_vaccinations) %>%
      summarise(
        .groups = "drop", count = n(), site = "overall",
        gender = "overall", age_at_screening_cat = "overall"
      )) %>%
    # NOTE(sen) Age
    bind_rows(data %>%
      group_by(age_at_screening_cat) %>%
      summarise(
        .groups = "drop", count = n(),
        gender = "overall", site = "overall", n_prior_vaccinations = "overall"
      )) %>%
    # NOTE(sen) Gender
    bind_rows(data %>%
      group_by(gender) %>%
      summarise(
        .groups = "drop", count = n(),
        age_at_screening_cat = "overall", site = "overall", n_prior_vaccinations = "overall"
      )) %>%
    # NOTE(sen) Site
    bind_rows(data %>%
      group_by(site) %>%
      summarise(
        count = n(), gender = "overall", age_at_screening_cat = "overall",
        n_prior_vaccinations = "overall"
      )) %>%
    # NOTE(sen) Total count
    bind_rows(data %>%
      summarise(
        count = n(), site = "overall", gender = "overall", age_at_screening_cat = "overall",
        n_prior_vaccinations = "overall"
      ))
}

participants_counts_long <- calc_counts(participants_with_extra)

participants_counts_wide <-
  participants_counts_long %>%
  pivot_wider(names_from = "site", values_from = "count")

write_csv(
  participants_counts_wide,
  "data-summary/counts-all.csv"
)

participants_currently_enrolled_counts_long <-
  calc_counts(participants_currently_enrolled)

participants_currently_enrolled_counts_wide <-
  participants_currently_enrolled_counts_long %>%
  pivot_wider(names_from = "site", values_from = "count")

write_csv(
  participants_currently_enrolled_counts_wide,
  "data-summary/counts-currently-enrolled.csv"
)

participants_currently_enrolled_recruited_in_2020_counts_long <-
  calc_counts(
    participants_currently_enrolled %>% filter(recruitment_year == 2020)
  )

participants_currently_enrolled_recruited_in_2020_counts_wide <-
  participants_currently_enrolled_recruited_in_2020_counts_long %>%
  pivot_wider(names_from = "site", values_from = "count")

write_csv(
  participants_currently_enrolled_recruited_in_2020_counts_wide,
  "data-summary/counts-currently-enrolled-recruited-in-2020.csv"
)

# SECTION Vaccination history for the nested study

prior_vaccinations_nested_only <- prior_vaccinations %>% filter(arm == "nested")

prior_vaccinations_nested_only_counts <- prior_vaccinations_nested_only %>%
  group_by(n_prior_vaccinations, site) %>%
  summarise(.groups = "drop", count = n()) %>%
  bind_rows(
    prior_vaccinations_nested_only %>%
      group_by(n_prior_vaccinations) %>%
      summarise(count = n(), site = "overall")
  ) %>%
  pivot_wider(names_from = "site", values_from = "count")

write_csv(prior_vaccinations_nested_only_counts, "data-summary/counts-prior-vaccinations-nested-only.csv")

# SECTION Swab counts

swabs_with_participant_info <- inner_join(swabs, participants %>% select(pid, site), "pid")

swabs_collected_only <- swabs_with_participant_info %>%
  filter(swab_collection == 1)

swab_collected_counts <- swabs_collected_only %>%
  group_by(site) %>%
  summarise(total_swabs = n(), unique_participants = length(unique(pid))) %>%
  bind_rows(
    swabs_collected_only %>%
      summarise(
        site = "overall",
        total_swabs = n(),
        unique_participants = length(unique(pid))
      )
  )

write_csv(swab_collected_counts, "data-summary/counts-swabs-collected.csv")
