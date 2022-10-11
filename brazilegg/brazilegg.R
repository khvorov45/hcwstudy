library(tidyverse)

brazilegg_raw <- readxl::read_excel("brazilegg/NIH_H1N1_ABrazil_11_egg_All.xlsx", na = c("", "repeat", "No result"))
vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())

age_groups_cutoffs <- c(-Inf, 30, 40, 50, Inf)

participants <- read_csv("data/participants.csv", col_types = cols()) %>%
	mutate(age_group = cut(age_screening, age_groups_cutoffs))

prior_counts <- vaccinations %>%
	filter(year == 2020) %>%
	group_by(pid) %>%
	summarise(prior_vacs2020 = sum(status %in% c("Australia", "Overseas")))

brazilegg <- brazilegg_raw %>%
	select(pid = PID, year = Year, day = Visit, titre = Titer) %>%
	mutate(timepoint = paste0(year, "-", day) %>%
		factor(c("2020-0", "2020-7", "2020-14", "2020-220", "2021-0", "2021-7", "2021-14", "2021-220"))) %>%
	filter(!is.na(titre)) %>%
	left_join(prior_counts, "pid") %>%
	left_join(participants %>% select(pid, age_screening, age_group), "pid") %>%
	mutate(year = as.integer(year))

titre_offsets <- brazilegg %>%
	select(pid, age_group) %>%
	distinct() %>%
	mutate(
		titre_offset = runif(n(), -0.1, 0.1),
		timepoint_offset = runif(n(), -0.1, 0.1),
		age_group_offset = as.integer(age_group) - (length(levels(age_group)) + 1) / 2
	) %>%
	select(-age_group)

titre_plot <- brazilegg %>%
	inner_join(titre_offsets, "pid") %>%
	mutate(
		titre_jit = exp(log(titre) + titre_offset),
		timepoint_jit = as.integer(timepoint) + age_group_offset * 0.15,
	) %>%
	ggplot(aes(timepoint_jit, titre_jit, color = age_group)) +
	theme_bw() +
	theme(
		panel.grid.minor = element_blank(),
		legend.position = "bottom",
	) +
	scale_y_log10("Titre", breaks = 5 * 2^(0:15)) +
	scale_x_continuous(
		"Timepoint",
		breaks = unique(sort(as.integer(brazilegg$timepoint))),
		labels = unique(sort(brazilegg$timepoint)),
	) +
	guides(color = guide_legend(override.aes = list(alpha = 1))) +
	#geom_line(aes(group = pid), alpha = 0.01) +
	#geom_point(alpha = 0.2, shape = 16) +
	geom_boxplot(aes(group = paste0(timepoint, age_group)), outlier.alpha = 1, fill = NA, outlier.size = 0.5)

ggsave("brazilegg/brazilegg.pdf", titre_plot, width = 15, height = 10, units = "cm")

brazilegg_ratios <- brazilegg %>%
	select(-timepoint) %>%
	pivot_wider(names_from = "day", values_from = "titre") %>%
	mutate(ratio = `14` / `0`) %>%
	filter(!is.na(ratio))

ratio_offsets <- brazilegg_ratios %>%
	select(pid) %>%
	distinct() %>%
	mutate(ratio_offset = runif(n(), -0.1, 0.1))

ratios_plot <- brazilegg_ratios %>%
	inner_join(ratio_offsets, "pid") %>%
	mutate(
		ratio_jit = exp(log(ratio) + ratio_offset),
		agegroup_jit = as.integer(age_group) + (year - 2020.5) * 0.4,
	) %>%
	ggplot(aes(agegroup_jit, ratio_jit, color = as.factor(year))) +
	theme_bw() +
	scale_y_log10("Ratio") +
	scale_color_discrete("Year") +
	scale_x_continuous("Age group", breaks = 1:4, labels = levels(brazilegg_ratios$age_group)) +
	geom_hline(yintercept = 1, lty = "11") +
	geom_line(aes(group = pid), alpha = 0.2, color = "black") +
	geom_point(alpha = 0.1, shape = 18, size = 3) +
	geom_boxplot(
		aes(group = paste0(age_group, year)),
		outlier.alpha = 0, fill = NA,
	)

ggsave("brazilegg/brazilegg-ratio.pdf", ratios_plot, width = 15, height = 10, units = "cm")

brazilegg_ratios_of_ratios <- brazilegg_ratios %>%
	select(-matches("^\\d{+}")) %>%
	pivot_wider(names_from = "year", values_from = "ratio") %>%
	mutate(ratio_of_ratios = `2021` / `2020`) %>%
	filter(!is.na(ratio_of_ratios))

ratio_of_ratios_plot <- brazilegg_ratios_of_ratios %>%
	ggplot(aes(age_group, ratio_of_ratios)) +
	scale_y_log10("Ratio of ratios (2021 / 2020)") +
	theme_bw() +
	geom_hline(yintercept = 1, lty = "11") +
	geom_jitter(width = 0.02, shape = 18, alpha = 0.1) +
	geom_boxplot(outlier.alpha = 0, fill = NA)

ggsave("brazilegg/brazilegg-ratio-of-ratios.pdf", ratio_of_ratios_plot, width = 10, height = 10, units = "cm")

serology <- read_csv("data/serology.csv", col_types = cols()) %>% left_join(participants, "pid")

ratios <- serology %>%
	filter(subtype == "H1") %>%
	# NOTE(sen) Only homologous
	filter((year == 2020 & str_detect(virus, "Brisbane")) | (year == 2021 & virus == str_detect(virus, "Victoria"))) %>%
	pivot_wider(names_from = "day", values_from = "titre") %>%
	mutate(ratio = `14` / `0`, virus_egg_cell = paste0("vaccine_", virus_egg_cell)) %>%
	select(pid, year, vaccine_ratio = ratio, virus_egg_cell) %>%
	pivot_wider(names_from = "virus_egg_cell", values_from = vaccine_ratio)

brazil_vaccine_ratios <- brazilegg_ratios %>%
	select(pid, year, prior_vacs2020, age_group, brazil_ratio = ratio) %>%
	inner_join(ratios, c("pid", "year")) %>%
	pivot_longer(c(brazil_ratio, vaccine_egg, vaccine_cell), names_to = "virus", values_to = "ratio") %>%
	mutate(virus = recode(virus, "brazil_ratio" = "brazil"))


summarise_logmean <- function(vec, round_to = 0) {
	vec <- na.omit(vec)
	total <- length(vec)
	log_vec <- log(vec)
	logmean <- mean(log_vec)
	logse <- sd(log_vec) / sqrt(total)
	logmargin <- 1.96 * logse
	loglow <- logmean - logmargin
	loghigh <- logmean + logmargin
	mean <- exp(logmean)
	low <- exp(loglow)
	high <- exp(loghigh)
	f <- \(x) round(x, round_to)
	string <- glue::glue("{f(mean)} ({f(low)}, {f(high)})")
	tibble(mean, low, high, string)
}

brazil_vaccine_gmrs <- brazil_vaccine_ratios %>%
	group_by(year, age_group, virus) %>%
	summarise(.groups = "drop", summarise_logmean(ratio, 2))

write_csv(brazil_vaccine_gmrs, "brazilegg/brazilegg_vaccine_gmrs.csv")

brazil_vaccine_ratios_plot <- brazil_vaccine_ratios %>%
	ggplot(aes(virus, ratio)) +
	theme_bw() +
	theme(
		panel.spacing = unit(0, "null"),
		strip.background = element_blank(),
	) +
	facet_grid(age_group ~ year) +
	scale_y_log10("Ratio (post/prevax)") +
	scale_x_discrete("Virus", labels = function(breaks) tools::toTitleCase(breaks) %>% str_replace("_", " ")) +
	coord_cartesian(ylim = c(1, 10)) +
	geom_hline(yintercept = 1, lty = "11") +
	geom_jitter(alpha = 0.1, shape = 18, width = 0.1) +
	geom_boxplot(outlier.alpha = 0, fill = NA, width = 0.2, color = "blue") +
	geom_pointrange(aes(y = mean, ymin = low, ymax = high), data = brazil_vaccine_gmrs, color = "red")

ggsave("brazilegg/brazilegg-vaccine-ratios.pdf", brazil_vaccine_ratios_plot, width = 15, height = 15, units = "cm")
