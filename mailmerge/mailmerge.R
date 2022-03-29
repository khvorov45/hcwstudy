library(tidyverse)

serology <- read_csv("data/serology.csv")

serology_wide <- serology %>%
	filter(day %in% c(0, 14, 220), virus_egg_cell == "egg", year == 2021) %>%
	mutate(
		subtype = recode(subtype, "H1" = "h1", "H3" = "h3", "BVic" = "bv", "BYam" = "by"),
		day_subtype = glue::glue("v{day}_{subtype}"),
		site = str_sub(pid, 0, 3),
	) %>%
	select(site, pid, day_subtype, titre) %>%
	pivot_wider(names_from = "day_subtype", values_from = "titre") %>%
	select(site, pid, contains("h3"), contains("h1"), contains("bv"), contains("by"))

unique(serology_wide$site)

bleed_dates <- read_csv("data/bleed-dates.csv")

bleed_dates_wide <- bleed_dates %>%
	filter(year == 2021) %>%
	mutate(day = recode(day, 
		"0" = "date_baseline_blood", "7" = "date_7d_blood", "14" = "date_14d_blood",
		"220" = "date_end_season_blood",
	)) %>%
	select(pid, date, day) %>%
	pivot_wider(names_from = "day", values_from = "date")

serology_and_bleed_dates <- serology_wide %>%
	left_join(bleed_dates_wide, "pid")

writexl::write_xlsx(serology_and_bleed_dates, "mailmerge/AllSites.xlsx")

serology_and_bleed_dates %>%
	group_by(site) %>%
	group_walk(function(data, key) {
		filename <- paste0("mailmerge/", key, ".xlsx")
		writexl::write_xlsx(data, filename)
	}, .keep = TRUE)
