library(tidyverse)

set.seed(1)

titres <- read_csv("data/serology.csv")

titre_presence <- titres %>%
 	group_by(pid, year) %>%
 	summarise(.groups = "drop", d0andd14 = all(c(0, 14) %in% day))

vaccinations <- read_csv("data/vaccinations.csv")

prior_vac_counts <- vaccinations %>%
    group_by(pid) %>%
    summarise(
        prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
    ) %>%
    pivot_longer(contains("prior"), names_to = "year", values_to = "prior_vacs5y") %>%
    mutate(year = str_replace(year, "prior", "") %>% parse_integer())

participants <- inner_join(titre_presence, prior_vac_counts, c("pid", "year"))

participants_relevant_subset <- participants %>%
	filter(d0andd14, prior_vacs5y %in% c(0, 5))

# NOTE(sen) See what the counts are like
participants_relevant_subset %>% count(year, prior_vacs5y)

participants_sample <- participants_relevant_subset %>%
	group_by(year, prior_vacs5y) %>%
	filter(row_number() %in% sample(1:n(), 16)) %>%
	ungroup() %>%
	arrange(year, prior_vacs5y, pid)

participants_relevant_subset %>%
	count(year, prior_vacs5y)

# NOTE(sen) Replace QCH-186 with someone naive from 2020 since we are out of
# naive in 2021
qch186_replacement <- participants_relevant_subset %>%
	filter(!pid %in% participants_sample$pid, prior_vacs5y == 0, year == 2020) %>%
	filter(row_number() %in% sample(1:n(), 1))

# NOTE(sen) Should be 16 per group
participants_sample %>% count(year, prior_vacs5y)

write_csv(participants_sample, "random128/random128.csv")
write_csv(qch186_replacement, "random128/qch186_replacement.csv")

titres_sample <- titres %>%
	inner_join(participants_sample %>% select(pid, year, prior_vacs5y), c("pid", "year")) %>%
	filter(day %in% c(0, 14), virus_egg_cell == "cell")

# NOTE(sen) Should be empty
titres_sample %>% group_by(pid, year, day, subtype) %>% filter(n() != 1)

sample_titres_plot <- titres_sample %>%
	ggplot(aes(day, titre, col = as.factor(prior_vacs5y))) +
	theme_bw() +
	theme(legend.position = "bottom") +
	scale_y_log10("Titre", breaks = 5 * 2^(0:15)) +
	scale_color_discrete("Prior vacs 5yr") +
	scale_x_continuous("Day", breaks = c(0, 14), expand = expansion(0.3)) +
	facet_grid(subtype ~ year) +
	geom_point(position = position_dodge(width = 4), alpha = 0.3, shape = 16) +
	geom_boxplot(
		aes(group = paste0(year, day, subtype, prior_vacs5y)),
		fill = NA, width = 2, outlier.alpha = 0, position = position_dodge(width = 4)
	)

ggsave("random128/random128titres.pdf", sample_titres_plot, width = 20, height = 20, units = "cm")

rises_sample <- titres_sample %>%
	pivot_wider(names_from = "day", values_from = "titre") %>%
	mutate(rise = `14` / `0`)

# NOTE(sen) Should be empty
rises_sample %>% group_by(pid, year, subtype) %>% filter(n() != 1)

# NOTE(sen) Missing d14 titres for everything except H1
rises_sample %>% filter(is.na(rise))
titres_sample %>% filter(pid == "QCH-186")

sample_rises_plot <- rises_sample %>%
	ggplot(aes(year, rise, col = as.factor(prior_vacs5y))) +
	theme_bw() +
	theme(legend.position = "bottom") +
	scale_y_log10("Rise", breaks = 2^(-15:15)) +
	scale_color_discrete("Prior vacs 5yr") +
	scale_x_continuous("Year", breaks = 2020:2021) +
	facet_wrap(~subtype, nrow = 1) +
	geom_point(position = position_dodge(width = 0.5), alpha = 0.3, shape = 16) +
	geom_boxplot(
		aes(group = paste0(year, subtype, prior_vacs5y)),
		fill = NA, width = 0.3, outlier.alpha = 0, position = position_dodge(width = 0.5)
	)

ggsave("random128/random128rises.pdf", sample_rises_plot, width = 20, height = 10, units = "cm")
