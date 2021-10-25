library(tidyverse)

participants <- read_csv("data/participants.csv", col_types = cols())

vaccinations <- read_csv("data/vaccinations.csv", col_types = cols()) %>%
  inner_join(participants %>% select(pid, recruitment_year), "pid")

prior_vaccination_counts <- vaccinations %>%
  # NOTE(sen) Only looking at the past 5 years prior to serology (only have 2020
  # serology right now)
  filter(
    year >= recruitment_year - 5,
    recruitment_year == 2020,
    year < recruitment_year
  ) %>%
  group_by(pid) %>%
  summarise(
    .groups = "drop",
    prior_vacs = sum(status %in% c("Australia", "Overseas"))
  )

viruses <- read_csv("data/viruses.csv", col_types = cols())

serology <- read_csv("data/serology.csv", col_types = cols()) %>%
  inner_join(participants, "pid") %>%
  inner_join(prior_vaccination_counts, "pid") %>%
  inner_join(viruses, c("virus" = "virus_name")) %>%
  mutate(
    timepoint = recode(
      day,
      "0" = "Pre-vax",
      "7" = "Post-vax (7 days)",
      "14" = "Post-vax (14 days)",
      "220" = "Post-season"
    ) %>%
      fct_reorder(day),
    virus_label_fct = factor(virus, levels = c(
      "A/Brisbane/02/2018", "A/Brisbane/02/2018 (IVR-190)",
      "A/South Australia/34/2019p", "A/South Australia/34/2019e",
      "B/Washington/02/2019", "B/Washington/02/2019e",
      "B/Phuket/3073/2013", "B/Phuket/3073/2013e"
    )),
    virus_label = paste0(virus_label_fct, " (", virus_clade, ")") %>%
      fct_reorder(as.integer(virus_label_fct)),
  )

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

titre_summary <- serology %>%
  filter(site == "perth") %>%
  group_by(virus_label, prior_vacs, timepoint) %>%
  summarise(.groups = "drop", summarise_logmean(titre))

titre_summary_wide <- titre_summary %>%
  select(virus_label, prior_vacs, string, timepoint) %>%
  pivot_wider(names_from = "prior_vacs", values_from = "string")

write_csv(titre_summary_wide, "data-summary/titre-summary.csv")

titre_plot <- serology %>%
  filter(site == "perth") %>%
  mutate(y_position = rnorm(n(), log(titre), 0.1) %>% exp()) %>%
  ggplot(aes(
    timepoint, y_position,
    col = as.factor(prior_vacs)
  )) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "null"),
    panel.spacing = unit(0, "null"),
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  scale_y_log10("Titre", breaks = 5 * 2^(0:15)) +
  scale_x_discrete("Timepoint") +
  scale_color_discrete("Prior vaccinations") +
  facet_wrap(~virus_label, nrow = 4) +
  guides(color = guide_legend(override.aes = list(alpha = 1), nrow = 1)) +
  geom_point(alpha = 0.3, position = position_dodge(width = 0.75), shape = 18) +
  geom_errorbar(
    data = titre_summary,
    aes(y = mean, ymin = low, ymax = high),
    position = position_dodge(width = 0.75),
    size = 1.2,
    width = 0.7
  ) +
  geom_point(
    data = titre_summary,
    aes(y = mean),
    position = position_dodge(width = 0.75),
    size = 4
  )

ggsave(
  "data-summary/titre-plot.pdf",
  titre_plot,
  unit = "cm", width = 35, height = 40
)

prevax_ratios <- serology %>%
  group_by(pid, virus) %>%
  filter("Pre-vax" %in% timepoint) %>%
  mutate(ratio_to_pre = titre / titre[timepoint == "Pre-vax"]) %>%
  ungroup() %>%
  filter(timepoint != "Pre-vax")

ratio_summary <- prevax_ratios %>%
  filter(site == "perth") %>%
  group_by(virus_label, prior_vacs, timepoint) %>%
  summarise(.groups = "drop", summarise_logmean(ratio_to_pre, 2))

ratio_summary_wide <- ratio_summary %>%
  select(virus_label, prior_vacs, timepoint, string) %>%
  pivot_wider(names_from = "prior_vacs", values_from = "string")

write_csv(ratio_summary_wide, "data-summary/ratio-summary.csv")

ratio_plot <- prevax_ratios %>%
  filter(site == "perth") %>%
  mutate(y_position = rnorm(n(), log(ratio_to_pre), 0.1) %>% exp()) %>%
  ggplot(aes(
    timepoint, y_position,
    col = as.factor(prior_vacs)
  )) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "null"),
    panel.spacing = unit(0, "null"),
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  scale_y_log10("Ratio to pre-vax", breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 10, 100)) +
  scale_x_discrete("Timepoint") +
  scale_color_discrete("Prior vaccinations") +
  scale_shape_discrete("Prior vaccinations") +
  facet_wrap(~virus_label, nrow = 4) +
  guides(color = guide_legend(override.aes = list(alpha = 1), nrow = 1)) +
  geom_point(alpha = 0.3, position = position_dodge(width = 0.75), shape = 18) +
  geom_errorbar(
    data = ratio_summary,
    aes(y = mean, ymin = low, ymax = high),
    position = position_dodge(width = 0.75),
    size = 1.2,
    width = 0.7
  ) +
  geom_point(
    data = ratio_summary,
    aes(y = mean),
    position = position_dodge(width = 0.75),
    size = 4
  )

ggsave(
  "data-summary/ratio-plot.pdf",
  ratio_plot,
  unit = "cm", width = 35, height = 40
)
