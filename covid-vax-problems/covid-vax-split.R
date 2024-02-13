library(tidyverse)

covid_vax <- readxl::read_excel("covid-vax-problems/covid-vax.xlsx")

covid_vax %>%
    filter(!is.na(Problem)) %>%
    mutate(site = str_sub(PID, 1, 3)) %>%
    group_split(site, .keep = TRUE) %>%
    walk(~write_csv(select(.x, -site), glue::glue("covid-vax-problems/{unique(.x$site)}.csv")))
