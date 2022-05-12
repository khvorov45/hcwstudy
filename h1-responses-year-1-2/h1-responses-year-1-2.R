library(tidyverse)

participants <- read_csv("data/participants.csv", col_types = cols())
vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())

calc_prior_vacs <- function(vaccinations, target_year) {
	vaccinations %>%
		filter(year < target_year, year >= target_year - 5) %>%
		group_by(pid) %>%
		summarise(prior_count = sum(status %in% c("Australia", "Overseas"))) %>%
		mutate(year = target_year)
}

prior_vacs <- calc_prior_vacs(vaccinations, 2020) %>%
	bind_rows(calc_prior_vacs(vaccinations, 2021))

hi <- read_csv("data/serology.csv", col_types = cols()) %>%
	left_join(read_csv("data/participants.csv", col_types = cols()), "pid") %>%
	left_join(prior_vacs, c("pid", "year")) %>%
	left_join(vaccinations %>% rename(vac_status = status), c("pid", "year")) %>%
	mutate(subtype = factor(subtype, c("H1", "H3", "BVic", "BYam")))

# NOTE(sen) There is one virus per subtype/year/egg status
hi %>%
	group_by(subtype, year, virus_egg_cell) %>%
	summarise(viruses = length(unique(virus)))

runif0 <- function(n, offset) runif(n, -offset, offset)

pid_jitter <- hi %>%
	select(pid) %>%
	distinct() %>%
	mutate(day_jitter = runif0(n(), 1), logtitre_jitter = runif0(n(), 0.1))

titre_subset_filter <- function(hi) {
	hi %>%
		filter(
			day != 7,
			subtype == "H1",
			# NOTE(sen) There is missing vaccination information, so just rely on
			# having (or not having) 14d titres
			#vac_status %in% c("Australia", "Overseas")
		)
}

pids_have_14d_titre <- hi %>%
	titre_subset_filter() %>%
	filter(day == 14, !is.na(titre)) %>%
	pull(pid)

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

hi %>%
	titre_subset_filter() %>%
	group_by(prior_count) %>%
	summarise(ind = length(unique(pid)))

gmts <- hi %>%
	titre_subset_filter() %>%
	group_by(year, day, subtype, prior_count, virus_egg_cell) %>%
	summarise(.groups = "drop", summarise_logmean(titre, 0))# %>%
	#filter(day == 14, virus_egg_cell == "egg")

gmrs <- hi %>%
	titre_subset_filter() %>%
	pivot_wider(names_from = "day", values_from = "titre") %>%
	mutate(ratio = `14` / `0`) %>%
	group_by(year, subtype, prior_count, virus_egg_cell) %>%
	summarise(.groups = "drop", summarise_logmean(ratio, 2))# %>%
	#filter(virus_egg_cell == "egg")

nonmissing_titre_counts <- hi %>%
	titre_subset_filter() %>%
	count(year, day, subtype, prior_count, virus_egg_cell)

summarise_prop <- function(success, total) {
  ci <- PropCIs::exactci(success, total, 0.95)
  prop <- success / total
  low <- ci$conf.int[[1]]
  high <- ci$conf.int[[2]]
  f <- function(x) round(x * 100)
  tibble(
    prop, low, high,
    comb = glue::glue("{f(prop)}% ({f(low)}%, {f(high)}%)")
  )
}

seroconv <- hi %>%
	titre_subset_filter() %>%
	pivot_wider(names_from = "day", values_from = "titre") %>%
	mutate(seroconv = `14` / `0` >= 4) %>%
	group_by(year, subtype, prior_count, virus_egg_cell) %>%
	filter(!is.na(seroconv)) %>%
	summarise(
		.groups = "drop",
		seroconv_count = sum(seroconv),
		total_count = n(),
		summarise_prop(seroconv_count, total_count)
	)# %>%
	#filter(virus_egg_cell == "cell")

seroprot <- hi %>%
	titre_subset_filter() %>%
	mutate(seroprot = titre >= 40) %>%
	group_by(year, subtype, prior_count, virus_egg_cell) %>%
	filter(!is.na(seroprot)) %>%
	summarise(
		.groups = "drop",
		seroconv_count = sum(seroprot),
		total_count = n(),
		summarise_prop(seroconv_count, total_count)
	)# %>%
	#filter(virus_egg_cell == "egg")

calc_day_fake <- function(day, prior_count) {
	if_else(day == 220, 50, day) + case_when(
		prior_count == 0 ~ -2,
		prior_count == 3 ~ 0,
		prior_count == 5 ~ 2,
		TRUE ~ 0
	)
}

make_titre_plot <- function(hi, gmts, nonmissing_titre_counts, prior_counts) {
	gmts_for_titre_plot <- gmts %>%
		mutate(day_fake = calc_day_fake(day, prior_count)) %>%
		filter(prior_count %in% prior_counts)

	counts_for_plot <- nonmissing_titre_counts %>%
		filter(prior_count %in% prior_counts) %>%
		inner_join(gmts_for_titre_plot, c("year", "day", "subtype", "prior_count", "virus_egg_cell"))

	hi %>%
		titre_subset_filter() %>%
		filter(pid %in% pids_have_14d_titre, prior_count %in% prior_counts) %>%
		left_join(pid_jitter, "pid") %>%
		mutate(
			day_fake = calc_day_fake(day, prior_count),
			day_jittered = day_fake + day_jitter,
			titre_jittered = exp(log(titre) + logtitre_jitter),
			alpha_line = case_when(
				prior_count == 0 & year == 2021 ~ 0.5,
				prior_count == 0 & year == 2020 ~ 0.4,
				prior_count == 5 & year == 2021 ~ 0.05,
				TRUE ~ 0.1
			),
			alpha_line = 0
		) %>%
		ggplot(aes(day_jittered, titre_jittered, col = as.factor(prior_count))) +
		theme_bw() +
		theme(
			panel.spacing.y = unit(0, "null"),
			legend.position = "bottom",
			axis.text.x = element_text(angle = 30, hjust = 1),
			panel.grid.minor = element_blank(),
		) +
		facet_grid(virus_egg_cell ~ year) +
		scale_y_log10("H1 GMT (95% CI)", breaks = 5 * 2^(0:15)) +
		scale_x_continuous(
			"Timepoint", breaks = c(0, 14, 50), labels = c("Pre-vax", "Post-vax", "Post-season")
		) +
		scale_alpha_identity(guide = "none") +
		scale_color_manual("Prior vaccinations", breaks = c(0, 3, 5), values = c("#ff69b4", "#61de2a", "#7b5804")) +
		geom_hline(yintercept = 40, lty = "11", alpha = 0.5) +
		geom_line(aes(group = paste0(pid, year, virus_egg_cell, subtype), alpha = alpha_line)) +
		geom_point(aes(alpha = alpha_line / 2), size = 1) +
		geom_pointrange(
			aes(day_fake, mean, ymin = low, ymax = high),
			data = gmts_for_titre_plot,
		) +
		geom_line(aes(day_fake, mean), data = gmts_for_titre_plot) +
		geom_text(
			aes(day_fake, mean, label = n), data = counts_for_plot,
			nudge_y = 0.15, hjust = 1, nudge_x = -0.5, show.legend = FALSE,
		)
}

ggsave(
	"h1-responses-year-1-2/titre_subset_plot_05.png",
	make_titre_plot(hi, gmts, nonmissing_titre_counts, c(0, 5)),
	width = 20, height = 20, units = "cm",
)

ggsave(
	"h1-responses-year-1-2/titre_subset_plot_035.png",
	make_titre_plot(hi, gmts, nonmissing_titre_counts, c(0, 3, 5)),
	width = 20, height = 20, units = "cm",
)

make_seroconv_plot <- function(seroconv, prior_counts) {
	seroconv %>%
		filter(prior_count %in% prior_counts) %>%
		ggplot(aes(virus_egg_cell, prop, col = as.factor(prior_count))) +
		theme_bw() +
		theme(
			panel.spacing.y = unit(0, "null"),
			legend.position = "bottom",
			axis.text.x = element_text(angle = 0, hjust = 0.5),
			panel.grid.minor = element_blank(),
		) +
		coord_cartesian(ylim = c(0, 1)) +
		facet_wrap(~ year, nrow = 1) +
		scale_y_continuous("H1 serconverted proportion (95% CI)", breaks = seq(0, 1, 0.1), expand = c(0, 0)) +
		scale_x_discrete("Virus") +
		scale_color_manual("Prior vaccinations", breaks = c(0, 3, 5), values = c("#ff69b4", "#61de2a", "#7b5804")) +
		geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 0.5)) +
		geom_text(
			aes(y = prop + 0.05, label = total_count),
			hjust = 1, show.legend = FALSE,
			position = position_dodge(width = 0.6)
		)
}

ggsave(
	"h1-responses-year-1-2/seroconv_subset_plot_05.png",
	make_seroconv_plot(seroconv, c(0, 5)),
	width = 18, height = 12, units = "cm",
)

ggsave(
	"h1-responses-year-1-2/seroconv_subset_plot_035.png",
	make_seroconv_plot(seroconv, c(0, 3, 5)),
	width = 18, height = 12, units = "cm",
)

seroconv %>%
	filter(prior_count %in% prior_counts)
