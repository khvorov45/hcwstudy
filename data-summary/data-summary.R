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

bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())

bleed_dates %>% filter(pid == "ALF-035")

bleed_offsets <- bleed_dates %>%
  group_by(year, pid) %>%
  mutate(days_from_baseline = (date - date[day == 0]) / lubridate::ddays(1)) %>%
  ungroup() %>%
  rename(bleed_date = date)

serology <- read_csv("data/serology.csv", col_types = cols()) %>%
  inner_join(participants, "pid") %>%
  inner_join(prior_vaccination_counts, "pid") %>%
  inner_join(bleed_offsets, c("pid", "year", "day")) %>%
  mutate(
    timepoint = recode(
      day,
      "0" = "Pre-vax",
      "7" = "Post-vax (7 days)",
      "14" = "Post-vax (14 days)",
      "220" = "Post-season"
    ) %>%
      fct_reorder(day),
    virus_fct = factor(virus, levels = c(
      "A/Brisbane/02/2018", "A/Brisbane/02/2018e",
      "A/South Australia/34/2019", "A/South Australia/34/2019e",
      "A/Victoria/2570/2019", "A/Victoria/2570/2019e",
      "A/Darwin/726/2019", "A/Hong Kong/2671/2019e",
      "B/Washington/02/2019", "B/Washington/02/2019e",
      "B/Phuket/3073/2013", "B/Phuket/3073/2013e"
    )),
    virus_label = paste0(virus, " (", subtype, ")"),
    virus_label_fct = fct_reorder(virus_label, as.integer(virus_fct)),
    subtype = factor(subtype, levels = c("H1", "H3", "BVic", "BYam"))
  )

serology %>%
  count(virus_label_fct, virus)

bleed_offsets_av <- bleed_offsets %>%
  group_by(day) %>%
  summarise(days_from_baseline_med = median(days_from_baseline, na.rm = TRUE))

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
  group_by(year, virus_label_fct, prior_vacs, timepoint, day, subtype, virus_egg_cell) %>%
  summarise(.groups = "drop", summarise_logmean(titre))

titre_summary_wide <- titre_summary %>%
  select(year, virus_label_fct, prior_vacs, string, timepoint) %>%
  pivot_wider(names_from = "prior_vacs", values_from = "string")

write_csv(titre_summary_wide, "data-summary/titre-summary.csv")

titre_plot <- serology %>%
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
    axis.text.x = element_text(angle = 15, hjust = 1),
  ) +
  scale_y_log10("Titre", breaks = 5 * 2^(0:15)) +
  scale_x_discrete("Timepoint") +
  scale_color_discrete("Prior vaccinations") +
  facet_wrap(~year + virus_label_fct, nrow = 4) +
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

# SECTION Multi-year serology

get_global_timepoint <- function(year, day) {
  paste0(year, "-", day) %>% fct_reorder(year * 1000 + day)
}

serology_multiyear <- serology %>%
  mutate(
    global_timepoint = get_global_timepoint(year, day),
    ypos = exp(runif(n(), log(titre) - 0.2, log(titre) + 0.2)),
    xpos = as.integer(global_timepoint),
    xpos_jit = runif(n(), xpos - 0.2, xpos + 0.2)
  )

titre_multi_year_plot <- serology_multiyear %>% 
  ggplot(aes(xpos_jit, ypos)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.spacing = unit(0, "null"),
  ) +
  scale_y_log10("Titre", breaks = 5 * 2^(0:10)) +
  scale_x_continuous(
    "Timepoint",
    breaks = unique(serology_multiyear$xpos),
    labels = unique(serology_multiyear$global_timepoint),
  ) +
  facet_grid(
    subtype + virus_egg_cell ~ prior_vacs,
    labeller = function(breaks) {
      if ("prior_vacs" %in% colnames(breaks)) {
        breaks$prior_vacs <- paste(breaks$prior_vacs, "prior")
      } else {
        breaks$subtype <- as.character(breaks$subtype)
      }
      breaks
    }
  ) +
  geom_line(aes(group = pid), alpha = 0.1, col = "gray50") + 
  geom_point(alpha = 0.1, shape = 18, col = "gray50") + 
  geom_line(
    aes(x = as.integer(global_timepoint), y = mean),
    titre_summary %>% mutate(global_timepoint = get_global_timepoint(year, day)),
    col = "hotpink"
  ) +
  geom_pointrange(
    aes(x = as.integer(global_timepoint), y = mean, ymin = low, ymax = high),
    titre_summary %>% mutate(global_timepoint = get_global_timepoint(year, day)),
    col = "hotpink", fatten = 1.2, size = 1,
  )

ggsave(
  "data-summary/titre-multiyear.pdf",
  titre_multi_year_plot,
  unit = "cm", width = 35, height = 40
)

# SECTION Other serology

serology2 <- serology %>%
  inner_join(bleed_offsets_av, "day") %>%
  filter(
    year == 2020,
    prior_vacs %in% c(0, 1, 5), str_detect(virus, "e$"), 
    timepoint != "Post-vax (7 days)"
  ) %>%
  mutate(
    y_position = rnorm(n(), log(titre), 0.1) %>% exp(),
    x_position = days_from_baseline_med + 4 * (as.integer(as.factor(prior_vacs)) - 2)
  )

colors2 <- c("#D39547", "#3F4393", "#319364")

fun_titre_plot2 <- function(data) {
  titre_summary2 <- data %>%
    group_by(virus_label, prior_vacs, timepoint, x_position, days_from_baseline_med) %>%
    summarise(.groups = "drop", summarise_logmean(titre))

  data %>%
    ggplot(aes(
      x_position, y_position,
      col = as.factor(prior_vacs)
    )) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box.spacing = unit(0, "null"),
      panel.spacing = unit(0, "null"),
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1)
    ) +
    scale_y_log10("Titre", breaks = 5 * 2^(0:15)) +
    scale_x_continuous(
      "Timepoint",
      breaks = bleed_offsets_av %>% filter(day != 7) %>% pull(days_from_baseline_med),
      labels = levels(fct_drop(serology2$timepoint))
    ) +
    scale_color_manual("Prior vaccinations", values = colors2) +
    facet_wrap(~virus_label, nrow = 2) +
    guides(color = guide_legend(override.aes = list(alpha = 1), nrow = 1)) +
    geom_hline(yintercept = 40, lty = "11", alpha = 0.5) +
    geom_point(alpha = 0.3, shape = 18) +
    geom_line(
      data = titre_summary2,
      aes(y = mean),
      size = 1
    ) +
    geom_errorbar(
      data = titre_summary2,
      aes(y = mean, ymin = low, ymax = high),
      size = 1.2,
      width = 0.1
    ) +
    geom_point(
      data = titre_summary2,
      aes(y = mean),
      size = 3
    )
}

titre_plot2 <- fun_titre_plot2(serology2)
titre_plot2_brisbane <- fun_titre_plot2(serology2 %>% filter(site == "brisbane"))

ggsave(
  "data-summary/titre-plot2.pdf",
  titre_plot2,
  unit = "cm", width = 25, height = 20
)

ggsave(
  "data-summary/titre-plot2-brisbane.pdf",
  titre_plot2_brisbane,
  unit = "cm", width = 25, height = 20
)

# SECTION Ratios

fun_prevax_rations <- function(data) {
  data %>%
    group_by(pid, virus, year) %>%
    filter("Pre-vax" %in% timepoint) %>%
    mutate(ratio_to_pre = titre / titre[timepoint == "Pre-vax"]) %>%
    ungroup() %>%
    filter(timepoint != "Pre-vax")
}

prevax_ratios <- fun_prevax_rations(serology)
prevax_ratios_brisbane <- fun_prevax_rations(serology %>% filter(site == "brisbane"))

ratio_summary <- prevax_ratios %>%
  group_by(year, virus_label, prior_vacs, timepoint) %>%
  summarise(.groups = "drop", summarise_logmean(ratio_to_pre, 2))

ratio_summary_brisbane <- prevax_ratios_brisbane %>%
  group_by(year, virus_label, prior_vacs, timepoint) %>%
  summarise(.groups = "drop", summarise_logmean(ratio_to_pre, 2))

ratio_summary_wide <- ratio_summary %>%
  select(year, virus_label, prior_vacs, timepoint, string) %>%
  pivot_wider(names_from = "prior_vacs", values_from = "string")

ratio_summary_wide_brisbane <- ratio_summary_brisbane %>%
  select(year, virus_label, prior_vacs, timepoint, string) %>%
  pivot_wider(names_from = "prior_vacs", values_from = "string")

write_csv(ratio_summary_wide, "data-summary/ratio-summary.csv")
write_csv(ratio_summary_wide_brisbane, "data-summary/ratio-summary-brisbane.csv")

fun_ratio_plot <- function(data) {
  data %>%
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
      axis.text.x = element_text(angle = 30, hjust = 1),
    ) +
    scale_y_log10("Ratio to pre-vax", breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 10, 100)) +
    scale_x_discrete("Timepoint") +
    scale_color_discrete("Prior vaccinations") +
    scale_shape_discrete("Prior vaccinations") +
    facet_wrap(~year + virus_label, nrow = 4) +
    coord_cartesian(ylim = c(0.1, 100)) + 
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
}

ratio_plot <- fun_ratio_plot(prevax_ratios)
ratio_plot_brisbane <- fun_ratio_plot(prevax_ratios_brisbane)

ggsave(
  "data-summary/ratio-plot.pdf",
  ratio_plot,
  unit = "cm", width = 35, height = 40
)

ggsave(
  "data-summary/ratio-plot-brisbane.pdf",
  ratio_plot_brisbane,
  unit = "cm", width = 25, height = 20
)

#
# SECTION Seroconversion
#

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

fun_seroconv_summary <- function(data) {
  data %>%
    select(-day, -bleed_date, -days_from_baseline) %>%
    filter(timepoint %in% c("Pre-vax", "Post-vax (14 days)")) %>%
    pivot_wider(names_from = timepoint, values_from = "titre") %>%
    mutate(seroconv = `Post-vax (14 days)` / `Pre-vax` >= 4) %>%
    filter(!is.na(seroconv)) %>%
    group_by(year, prior_vacs, virus, virus_egg_cell, virus_label_fct, subtype) %>%
    summarise(.groups = "drop", total = n(), n_seroconv = sum(seroconv)) %>%
    mutate(props = map2(n_seroconv, total, summarise_prop)) %>%
    unnest(props)
}

seroconv_summary <- fun_seroconv_summary(serology)
seroconv_summary_brisbane <- fun_seroconv_summary(serology %>% filter(site == "brisbane"))

write_csv(seroconv_summary, "data-summary/seroconv.csv")
write_csv(seroconv_summary_brisbane, "data-summary/seroconv-brisbane.csv")

seroconv_plot <- seroconv_summary %>% 
  mutate(
    subtype_lbl = paste(subtype, virus_egg_cell) %>% 
      fct_reorder(as.integer(subtype))
  ) %>%
  ggplot(aes(subtype_lbl, prop, ymin = low, ymax = high, col = as.factor(year))) +
  theme_bw() +
  theme(
    panel.spacing = unit(0, "null"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom",
  ) +
  facet_wrap(
    ~ prior_vacs, ncol = 1, strip.position = "right",
    labeller = as_labeller(~paste(.x, "prior"))
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_discrete("Subtype") +
  scale_y_continuous("Seroconverted") +
  scale_color_discrete("Year") +
  geom_pointrange(position = position_dodge(width = 0.8))

ggsave(
  "data-summary/seroconv.pdf",
  seroconv_plot,
  units = "cm", width = 15, height = 30,
)

fun_seroconv_plot2 <- function(data) {
  data %>%
    filter(year == 2020, virus_egg_cell == "egg", prior_vacs %in% c(0, 1, 5)) %>%
    ggplot(aes(virus_label_fct, prop, ymin = low, ymax = high, col = as.factor(prior_vacs))) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 30, hjust = 1),
      plot.margin = margin(5, 5, 5, 25),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous("Seroconverted", breaks = seq(0, 1, 0.1), labels = scales::percent_format(accuracy = 1)) +
    scale_x_discrete("Virus") +
    scale_color_manual("Prior vaccinations", values = colors2) +
    geom_errorbar(position = position_dodge(width = 0.5), width = 0.4) +
    geom_point(position = position_dodge(width = 0.5))
}

seroconv_plot2 <- fun_seroconv_plot2(seroconv_summary)
seroconv_plot2_brisbane <- fun_seroconv_plot2(seroconv_summary_brisbane)

ggsave("data-summary/seroconv2.pdf", seroconv_plot2, width = 15, height = 10, units = "cm")
ggsave("data-summary/seroconv2-brisbane.pdf", seroconv_plot2_brisbane, width = 15, height = 10, units = "cm")

write_csv(serology %>% filter(site == "brisbane"), "data-summary/serology-brisbane.csv")
