library(tidyverse)
library(officer)

serology <- read_csv("data/serology.csv", col_types = cols())

serology %>% count(virus, subtype, year) %>% arrange(year, virus, subtype) %>% print(n = 300)

serology_wide <- serology %>%
	filter(
		vax_inf == "V", virus_vaccine, day %in% c(0, 14, 220), virus_egg_cell == "egg",
	) %>%
	mutate(
		subtype = recode(subtype, "H1" = "h1", "H3" = "h3", "BVic" = "bv", "BYam" = "by"),
		day_subtype = glue::glue("v{day}_{subtype}"),
		site = str_sub(pid, 0, 3),
	) %>%
	select(year, site, pid, day_subtype, titre) %>%
	pivot_wider(names_from = "day_subtype", values_from = "titre") %>%
	select(year, site, pid, contains("h3"), contains("h1"), contains("bv"), contains("by"))

unique(serology_wide$site)

bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())

bleed_dates_wide <- bleed_dates %>%
	mutate(day = recode(day,
		"0" = "date_baseline_blood", "7" = "date_7d_blood", "14" = "date_14d_blood",
		"220" = "date_end_season_blood",
	)) %>%
	select(pid, year, date, day) %>%
	pivot_wider(names_from = "day", values_from = "date")

serology_and_bleed_dates <- serology_wide %>%
	left_join(bleed_dates_wide, c("pid", "year"))

writexl::write_xlsx(serology_and_bleed_dates, "mailmerge/AllSitesAllYears.xlsx")

serology_and_bleed_dates %>%
	group_by(site) %>%
	group_walk(function(data, key) {
		print(key)
		filename <- paste0("mailmerge/", key, "AllYears.xlsx")
		writexl::write_xlsx(data, filename)
	}, .keep = TRUE)

#
# SECTION Generate the PDFs
#

site_year_names <- serology_and_bleed_dates %>% select(site, year) %>% distinct() %>% mutate(siteyear = paste0(site, "-", year)) %>% pull(siteyear)

output_dirs <- paste0("mailmerge/", site_year_names)
walk(output_dirs, unlink, recursive = TRUE)
walk(output_dirs, dir.create)

vaccine_viruses <- serology %>% 
	filter(virus_vaccine, virus_egg_cell == "egg") %>% 
	select(virus, subtype, year) %>% 
	distinct() %>% 
	mutate(subtype = factor(subtype, c("H1", "H3", "BVic", "BYam"))) %>%
	arrange(year, subtype)

today_date <- as.character(Sys.Date())

serology_and_bleed_dates %>%
	# group_by(site, year) %>%
	# filter(pid == first(pid)) %>%
	group_by(pid, year) %>%
	group_walk(function(data, key) {
		pid_first3 <- str_sub(key$pid, 1, 3)
		this_year_vaccine_viruses <- vaccine_viruses %>% filter(year == key$year) %>% mutate(subtype = as.character(subtype))

		pid_doc <- read_docx(glue::glue("mailmerge/{pid_first3}-{key$year}.docx")) %>%
			body_replace_text_at_bkm("PID", key$pid) %>%
			body_replace_text_at_bkm("REPORTDATE", today_date) %>%
			body_replace_text_at_bkm("YEAR1", as.character(key$year)) %>%
			body_replace_text_at_bkm("YEAR2", as.character(key$year)) %>%
			body_replace_text_at_bkm("STRAIN1", this_year_vaccine_viruses$virus[[1]]) %>%
			body_replace_text_at_bkm("STRAIN2", this_year_vaccine_viruses$virus[[2]]) %>%
			body_replace_text_at_bkm("STRAIN3", this_year_vaccine_viruses$virus[[3]]) %>%
			body_replace_text_at_bkm("SUBTYPE1", this_year_vaccine_viruses$subtype[[1]]) %>%
			body_replace_text_at_bkm("SUBTYPE2", this_year_vaccine_viruses$subtype[[2]]) %>%
			body_replace_text_at_bkm("SUBTYPE3", this_year_vaccine_viruses$subtype[[3]]) %>%
			body_replace_text_at_bkm("BASELINEDATE", as.character(data$date_baseline_blood)) %>%
			body_replace_text_at_bkm("POSTVAXDATE", as.character(data$date_14d_blood)) %>%
			body_replace_text_at_bkm("ENDDATE", as.character(data$date_end_season_blood)) %>%
			body_replace_text_at_bkm("B1", as.character(data$v0_h1)) %>%
			body_replace_text_at_bkm("B2", as.character(data$v0_h3)) %>%
			body_replace_text_at_bkm("B3", as.character(data$v0_bv)) %>%
			body_replace_text_at_bkm("PV1", as.character(data$v14_h1)) %>%
			body_replace_text_at_bkm("PV2", as.character(data$v14_h3)) %>%
			body_replace_text_at_bkm("PV3", as.character(data$v14_bv)) %>%
			body_replace_text_at_bkm("E1", as.character(data$v220_h1)) %>%
			body_replace_text_at_bkm("E2", as.character(data$v220_h3)) %>%
			body_replace_text_at_bkm("E3", as.character(data$v220_bv))

		if (key$year == 2020 | key$year == 2021) {
			pid_doc <- pid_doc %>%
				body_replace_text_at_bkm("STRAIN4", this_year_vaccine_viruses$virus[[4]]) %>%
				body_replace_text_at_bkm("SUBTYPE4", this_year_vaccine_viruses$subtype[[4]]) %>%
				body_replace_text_at_bkm("B4", as.character(data$v0_by)) %>%
				body_replace_text_at_bkm("PV4", as.character(data$v14_by)) %>%
				body_replace_text_at_bkm("E4", as.character(data$v220_by))
		}

		filename <- file.path("mailmerge", paste0(pid_first3, "-", key$year), paste0(key$pid, ".docx"))
		print(pid_doc, target = filename)
	})
