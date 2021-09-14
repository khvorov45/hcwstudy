# Upload the viruses and the serology data to the reports

library(tidyverse)

db_url <- function(path) paste0("localhost:7001/", path)
# Note that anyone with this token has access to the database
auth_header <- httr::add_headers(
  Authorization = "Bearer"
)

# Viruses =====================================================================

viruses <- read_csv("NIH Viruses.csv", col_types = cols()) %>%
  select(name = Virus_Name, shortName = Short_Name, clade = Clade)

# Titres ======================================================================

read_one <- function(path) {
  read_csv(path, col_types = cols()) %>%
    mutate(
      redcapProjectYear = 2020,
      day = str_replace(Time, "V|v", "") %>% as.integer(),
      path = path
    ) %>%
    select(
      pid = PID, redcapProjectYear, day, virus = Target,
      titre = Titer, contains("PID_time"), path
    ) %>%
    # The whole point is to have a titre measurement, no titre - don't insert
    # into the table at all
    filter(!is.na(titre))
}

all_tables <- list.files(pattern = "HI_") %>% map_dfr(read_one)

all_tables %>% filter(!complete.cases(select(., -PID_time)))

# Let's assume JHH-018's titres are all at V0
all_tables_fix_day <- all_tables %>% mutate(
  day = if_else(pid == "JHH-018", 0L, day)
)
all_tables_fix_day %>% filter(!complete.cases(select(., -PID_time)))

# See if viruses match
serology_viruses <- unique(all_tables_fix_day$virus)
serology_viruses[!serology_viruses %in% viruses$name]

# Fix virus names
all_tables_fix_viruses <- all_tables_fix_day %>%
  mutate(
    virus = virus %>%
      # Inconsistent left-pad
      str_replace("/(\\d)/", "/0\\1/") %>%
      # Let's assume that "c" at the end is not important because it's not
      # in the virus table
      str_replace("c$", "") %>%
      # Inconsistent egg vs e
      str_replace("egg$", "e") %>%
      # Inconsistent south australia
      str_replace("SouthAustralia|SouthAust", "South Australia") %>%
      # Inconsistent IVR format
      str_replace("IVR190", "(IVR-190)") %>%
      # Of course there's gonna be a space before e
      str_replace(" e$", "e") %>%
      # This is just to mess with me personally I guess
      str_replace("SthAust_34_19Cell", "A/South Australia/34/2019"),
    # Washington egg doesn't have an e at the end BUT ONLY SOMETIMES
    virus = if_else(
      str_detect(path, "B_Vicegg") & !str_detect(virus, "e$"),
      paste0(virus, "e"),
      virus
    ),
    # Let's assume south australia 2019 is actually a 'p' as per the
    # virus table
    virus = recode(
      virus,
      "A/South Australia/34/2019" = "A/South Australia/34/2019p"
    ),
  )

# Make sure the viruses match now
serology_viruses_fixed <- unique(all_tables_fix_viruses$virus)
serology_viruses_fixed[!serology_viruses_fixed %in% viruses$name]

# See if PID's match

participants <- httr::GET(
  db_url("participants"),
  auth_header
) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  as_tibble()

unique(
  all_tables_fix_viruses$pid[!all_tables_fix_viruses$pid %in% participants$pid]
)

all_tables_fix_pids <- all_tables_fix_viruses %>%
  # WCH-025 became WCH-818 and we seem to have V0 WCH-818 data
  filter(pid != "WCH-025")

unique(
  all_tables_fix_pids$pid[!all_tables_fix_pids$pid %in% participants$pid]
)

# Look for duplicate keys
all_tables_fix_pids %>%
  group_by(pid, redcapProjectYear, day, virus) %>%
  filter(n() > 1) %>%
  arrange(pid, redcapProjectYear, day, virus)

# Delete the existing data
httr::DELETE(
  db_url("virus/all"),
  auth_header
)
httr::DELETE(
  db_url("serology/all"),
  auth_header
)

# Upload new
httr::POST(
  db_url("virus"),
  auth_header,
  body = viruses,
  encode = "json"
)
httr::POST(
  db_url("serology"),
  auth_header,
  body = all_tables_fix_pids,
  encode = "json"
)

# Save what's been uploaded
viruses %>%
  jsonlite::toJSON() %>%
  write("export-virus.json")
all_tables_fix_pids %>%
  jsonlite::toJSON() %>%
  write("export-serology.json")
