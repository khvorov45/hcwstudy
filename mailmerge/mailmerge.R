library(tidyverse)
library(officer)

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

#
# SECTION Generate the PDFs
#

site_names <- unique(serology_and_bleed_dates$site)
site_names <- "PCH"

output_dirs <- paste0("mailmerge/", site_names)
walk(output_dirs, unlink, recursive = TRUE)
walk(output_dirs, dir.create)

strains2021 <- c(
	"A/Victoria/2570/2019",
	"A/Hong Kong/2671/2019",
	"B/Washington/02/2019",
	"B/Phuket/3073/2013"
)

subtypes <- c("A(H1N1)pdm09", "A(H3N2)", "B/Victoria", "B/Yamagata")

today_date <- as.character(Sys.Date())
vaccine_year <- as.character(lubridate::year(Sys.Date()) - 1)

serology_and_bleed_dates %>%
	filter(site %in% site_names) %>%
	#group_by(site) %>%
	#filter(pid == first(pid)) %>%
	group_by(pid) %>%
	group_walk(function(data, key) {
		pid <- key$pid
		pid_first3 <- str_sub(pid, 1, 3)

		pid_doc <- read_docx(glue::glue("mailmerge/{pid_first3}.docx")) %>%
			body_replace_text_at_bkm("PID", pid) %>%
			body_replace_text_at_bkm("REPORTDATE", today_date) %>%
			body_replace_text_at_bkm("YEAR1", vaccine_year) %>%
			body_replace_text_at_bkm("YEAR2", vaccine_year) %>%
			body_replace_text_at_bkm("STRAIN1", strains2021[[1]]) %>%
			body_replace_text_at_bkm("STRAIN2", strains2021[[2]]) %>%
			body_replace_text_at_bkm("STRAIN3", strains2021[[3]]) %>%
			body_replace_text_at_bkm("STRAIN4", strains2021[[4]]) %>%
			body_replace_text_at_bkm("SUBTYPE1", subtypes[[1]]) %>%
			body_replace_text_at_bkm("SUBTYPE2", subtypes[[2]]) %>%
			body_replace_text_at_bkm("SUBTYPE3", subtypes[[3]]) %>%
			body_replace_text_at_bkm("SUBTYPE4", subtypes[[4]]) %>%
			body_replace_text_at_bkm("BASELINEDATE", as.character(data$date_baseline_blood)) %>%
			body_replace_text_at_bkm("POSTVAXDATE", as.character(data$date_14d_blood)) %>%
			body_replace_text_at_bkm("ENDDATE", as.character(data$date_end_season_blood)) %>%
			body_replace_text_at_bkm("B1", as.character(data$v0_h1)) %>%
			body_replace_text_at_bkm("B2", as.character(data$v0_h3)) %>%
			body_replace_text_at_bkm("B3", as.character(data$v0_bv)) %>%
			body_replace_text_at_bkm("B4", as.character(data$v0_by)) %>%
			body_replace_text_at_bkm("PV1", as.character(data$v14_h1)) %>%
			body_replace_text_at_bkm("PV2", as.character(data$v14_h3)) %>%
			body_replace_text_at_bkm("PV3", as.character(data$v14_bv)) %>%
			body_replace_text_at_bkm("PV4", as.character(data$v14_by)) %>%
			body_replace_text_at_bkm("E1", as.character(data$v220_h1)) %>%
			body_replace_text_at_bkm("E2", as.character(data$v220_h3)) %>%
			body_replace_text_at_bkm("E3", as.character(data$v220_bv)) %>%
			body_replace_text_at_bkm("E4", as.character(data$v220_by))

		filename <- file.path("mailmerge", pid_first3, paste0(pid, ".docx"))
		print(pid_doc, target = filename)
	})
