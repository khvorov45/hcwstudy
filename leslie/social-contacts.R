library(tidyverse)

redcap_tokens <- read_csv("data-raw/redcap-tokens.csv", col_types = cols())

redcap_request <- function(project_year, event, fields, ...) {
  httr::POST(
    "https://biredcap.mh.org.au/api/",
    body = list(
      token = redcap_tokens %>% filter(year == project_year) %>% pull(token),
      content = "record",
      events = event,
      fields = fields,
      format = "csv",
      ...
    )
  ) %>%
    httr::content(as = "text") %>%
    read_csv(col_types = cols(), guess_max = 1e5) %>%
    mutate(redcap_project_year = project_year)
}

contacts <- redcap_request(
    2023, "baseline_arm_1", 
    paste0(c("record_id", "pid", paste0(c("age", "gender", "interaction", "location", "known"), "_contact1")), collapse = ",")
)

contacts %>%
    select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance, -redcap_project_year) %>%
    pivot_longer(c(-record_id, -pid), names_to = "varname", values_to = "varvalue") %>%
    summarise(.by = c(record_id, pid), anynotmissing = any(!is.na(varvalue))) %>%
    filter(anynotmissing) %>%
    select(-anynotmissing) %>%
    mutate(pid_first3 = str_sub(pid, 1, 3) %>% toupper()) %>%
    group_by(pid_first3) %>%
    group_walk(function(data, key) {
        write_csv(data, glue::glue("leslie/social_contacts_completed-{key$pid_first3}.csv"))
    })

