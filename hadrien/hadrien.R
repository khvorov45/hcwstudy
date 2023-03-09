library(tidyverse)

serology <- read_csv("data/serology.csv") %>% filter(vax_inf == "I")

bleed_dates <- read_csv("data/postinf-bleed-dates.csv")

serology %>%
    filter(pid == "CHW-068")

serology %>%
    summarise(pids = length(unique(pid)))

surveys <- read_csv("data/weekly-surveys.csv")

daily <- read_csv("data/daily-surveys.csv")

serology_with_bleed_dates <- serology %>%
    left_join(
        bleed_dates %>% 
            select(pid, year, day, bleed_date) %>% 
            distinct() %>%
            group_by(pid, year, day) %>% 
            filter(n() == 1) %>%
            ungroup(),
        c("pid", "year", "day")
    ) %>%
    filter(!is.na(bleed_date))

surveys %>%
    filter(pid == "ALF-016", year == 2022) %>%
    print(n = 100)

daily %>% filter(pid == "ALF-016")

surveys %>%
    filter(pid == "ALF-057", year == 2022) %>%
    print(n = 100)

result <- serology_with_bleed_dates %>%
    left_join(
        surveys %>%
            filter(
                (pid == "ALF-016" & year == 2022 & survey_index == 19)
                | (pid == "ALF-057" & year == 2022 & survey_index == 18)
            ),
        c("pid", "year")
    ) %>%
    mutate(
        symptom_duration = case_when(
            pid == "ALF-016" ~ 7,
            pid == "ALF-057" ~ 14,
        )
    )

write_csv(result, "temp.csv")
