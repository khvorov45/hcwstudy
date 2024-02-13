library(tidyverse)

participants <- read_csv("data/participants.csv", col_types = cols())

vaccinations <- read_csv("data/vaccinations.csv", col_types = cols()) %>%
  inner_join(participants %>% select(pid, recruitment_year), "pid")

prior_vaccination_counts_2020 <- vaccinations %>%
  filter(year >= 2020 - 5, year < 2020) %>%
  group_by(pid) %>%
  summarise(
    .groups = "drop",
    prior_vacs = sum(status %in% c("Australia", "Overseas"))
  )

prior_vaccination_counts_2021 <- vaccinations %>%
  filter(year >= 2021 - 5, year < 2021) %>%
  group_by(pid) %>%
  summarise(
    .groups = "drop",
    prior_vacs = sum(status %in% c("Australia", "Overseas"))
  )

prior_vaccination_counts <- bind_rows(
  prior_vaccination_counts_2020 %>% mutate(year = 2020),
  prior_vaccination_counts_2021 %>% mutate(year = 2021),
)

setdiff(prior_vaccination_counts$pid, participants$pid)
setdiff(participants$pid, prior_vaccination_counts$pid)

vaccinations_study_years <- vaccinations %>% 
  filter(year %in% c(2020, 2021)) %>% 
  mutate(vaccinated = status %in% c("Australia", "Overseas")) %>%
  select(pid, year, vaccinated)

bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())

bleed_dates %>% filter(pid == "ALF-035")

bleed_offsets <- bleed_dates %>%
  group_by(year, pid) %>%
  mutate(days_from_baseline = (date - date[day == 0]) / lubridate::ddays(1)) %>%
  ungroup() %>%
  rename(bleed_date = date)

serology <- read_csv("data/serology.csv", col_types = cols()) %>%
  inner_join(participants, "pid") %>%
  left_join(prior_vaccination_counts, c("pid", "year")) %>%
  left_join(vaccinations_study_years, c("pid", "year")) %>%
  left_join(bleed_offsets, c("pid", "year", "day")) %>%
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
  group_by(vaccinated, year, virus_label_fct, prior_vacs, timepoint, day, subtype, virus_egg_cell) %>%
  summarise(.groups = "drop", summarise_logmean(titre))

titre_summary_wide <- titre_summary %>%
  select(vaccinated, year, virus_label_fct, prior_vacs, string, timepoint) %>%
  pivot_wider(names_from = "prior_vacs", values_from = "string")

write_csv(titre_summary_wide, "data-summary/titre-summary.csv")

fun_titre_plot <- function(titres, titre_summary) {
  titres %>%
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
}

titre_plot_vaccinated <- fun_titre_plot(
  serology %>% filter(vaccinated), 
  titre_summary %>% filter(vaccinated)
)

ggsave(
  "data-summary/titre-plot-vaccinated.pdf",
  titre_plot_vaccinated,
  unit = "cm", width = 35, height = 40
)

# SECTION Multi-year serology

get_global_timepoint <- function(year, day) {
  paste0(year, "-", day)
}

serology_multiyear <- serology %>%
  mutate(
    global_timepoint = get_global_timepoint(year, day) %>% fct_reorder(year * 1000 + day),
    ypos = exp(runif(n(), log(titre) - 0.2, log(titre) + 0.2)),
    xpos = as.integer(global_timepoint),
    xpos_jit = runif(n(), xpos - 0.2, xpos + 0.2)
  )

fun_titre_multiyear_plot <- function(serology_multiyear, titre_summary, alpha = 0.1) {
  titre_summary <- titre_summary %>% mutate(
    global_timepoint = get_global_timepoint(year, day) %>% 
      factor(levels(serology_multiyear$global_timepoint))
  )

  serology_multiyear %>% 
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
    geom_line(aes(group = pid), alpha = alpha, col = "gray50") + 
    geom_point(alpha = alpha, shape = 18, col = "gray50") + 
    geom_line(
      aes(x = as.integer(global_timepoint), y = mean),
      titre_summary,
      col = "hotpink"
    ) +
    geom_pointrange(
      aes(x = as.integer(global_timepoint), y = mean, ymin = low, ymax = high),
      titre_summary,
      col = "hotpink", fatten = 1.2, size = 1,
    )
}

titre_multi_year_vaccinated_plot <- fun_titre_multiyear_plot(
  serology_multiyear %>% filter(vaccinated),
  titre_summary %>% filter(vaccinated)
)

titre_multi_year_unvaccinated_plot <- fun_titre_multiyear_plot(
  serology_multiyear %>% filter(!vaccinated),
  titre_summary %>% filter(!vaccinated),
  alpha = 0.7
)

ggsave(
  "data-summary/titre-multiyear-vaccinated.pdf",
  titre_multi_year_vaccinated_plot,
  unit = "cm", width = 35, height = 40
)

ggsave(
  "data-summary/titre-multiyear-unvaccinated.pdf",
  titre_multi_year_unvaccinated_plot,
  unit = "cm", width = 35, height = 40
)

# SECTION Other serology

serology2 <- serology %>%
  inner_join(bleed_offsets_av, "day") %>%
  filter(
    str_detect(virus, "e$"), 
    timepoint != "Post-vax (7 days)"
  ) %>%
  mutate(
    y_position = rnorm(n(), log(titre), 0.1) %>% exp(),
    x_position = days_from_baseline_med + 4 * (as.integer(as.factor(prior_vacs)) - 2)
  )

colors2 <- c("#D39547", "#3F4393", "#319364")

fun_titre_plot2 <- function(data) {
  titre_summary2 <- data %>%
    group_by(virus_label_fct, prior_vacs, timepoint, x_position, days_from_baseline_med) %>%
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
    facet_wrap(~virus_label_fct, nrow = 2) +
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

titre_plot2_015_2020_vaccinated <- fun_titre_plot2(
  serology2 %>% filter(vaccinated, year == 2020, prior_vacs %in% c(0, 1, 5))
)

titre_plot2_035_2020_vaccinated <- fun_titre_plot2(
  serology2 %>% filter(vaccinated, year == 2020, prior_vacs %in% c(0, 3, 5))
)

titre_plot2_015_2021_vaccinated <- fun_titre_plot2(
  serology2 %>% filter(vaccinated, year == 2021, prior_vacs %in% c(0, 1, 5))
)

titre_plot2_035_2021_vaccinated <- fun_titre_plot2(
  serology2 %>% filter(vaccinated, year == 2021, prior_vacs %in% c(0, 3, 5))
)

ggsave(
  "data-summary/titre-plot2-015-2020-vaccinated.pdf",
  titre_plot2_015_2020_vaccinated,
  unit = "cm", width = 25, height = 20
)

ggsave(
  "data-summary/titre-plot2-035-2020-vaccinated.pdf",
  titre_plot2_035_2020_vaccinated,
  unit = "cm", width = 25, height = 20
)

ggsave(
  "data-summary/titre-plot2-015-2021-vaccinated.pdf",
  titre_plot2_015_2021_vaccinated,
  unit = "cm", width = 25, height = 20
)

ggsave(
  "data-summary/titre-plot2-035-2021-vaccinated.pdf",
  titre_plot2_035_2021_vaccinated,
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

ratio_summary <- prevax_ratios %>%
  group_by(vaccinated, year, virus_label_fct, prior_vacs, timepoint) %>%
  summarise(.groups = "drop", summarise_logmean(ratio_to_pre, 2))

ratio_summary_wide <- ratio_summary %>%
  select(vaccinated, year, virus_label_fct, prior_vacs, timepoint, string) %>%
  pivot_wider(names_from = "prior_vacs", values_from = "string")

write_csv(ratio_summary_wide, "data-summary/ratio-summary.csv")

fun_ratio_plot <- function(ratio, ratio_summary) {
  ratio %>%
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
    facet_wrap(~year + virus_label_fct, nrow = 4) +
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

ratio_plot_vaccinated <- fun_ratio_plot(
  prevax_ratios %>% filter(vaccinated), 
  ratio_summary %>% filter(vaccinated)
)

ggsave(
  "data-summary/ratio-plot-vaccinated.pdf",
  ratio_plot_vaccinated,
  unit = "cm", width = 35, height = 40
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
    group_by(vaccinated, year, prior_vacs, virus, virus_egg_cell, virus_label_fct, subtype) %>%
    summarise(.groups = "drop", total = n(), n_seroconv = sum(seroconv)) %>%
    mutate(props = map2(n_seroconv, total, summarise_prop)) %>%
    unnest(props) %>%
    arrange(vaccinated, year, prior_vacs, virus_label_fct)
}

seroconv_summary <- fun_seroconv_summary(serology)

write_csv(seroconv_summary, "data-summary/seroconv.csv")

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
  facet_grid(
    vaccinated ~ prior_vacs,
    labeller = function(breaks) {
      if ("vaccinated" %in% colnames(breaks)) {
        breaks$vaccinated <- if_else(breaks$vaccinated, "Vaccinated", "Unvaccinated")
      } else {
        breaks$prior_vacs <- paste(breaks$prior_vacs, "prior")
      }
      breaks
    }
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_discrete("Subtype") +
  scale_y_continuous("Seroconverted") +
  scale_color_discrete("Year") +
  geom_pointrange(position = position_dodge(width = 0.8))

ggsave(
  "data-summary/seroconv.pdf",
  seroconv_plot,
  units = "cm", width = 35, height = 20,
)

fun_seroconv_plot2 <- function(data) {
  data %>%
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

seroconv_plot2_2020_egg_015_vac <- fun_seroconv_plot2(
  seroconv_summary %>% filter(vaccinated, year == 2020, virus_egg_cell == "egg", prior_vacs %in% c(0, 1, 5))
)

seroconv_plot2_2021_egg_015_vac <- fun_seroconv_plot2(
  seroconv_summary %>% filter(vaccinated, year == 2021, virus_egg_cell == "egg", prior_vacs %in% c(0, 1, 5))
)

seroconv_plot2_2020_egg_035_vac <- fun_seroconv_plot2(
  seroconv_summary %>% filter(vaccinated, year == 2020, virus_egg_cell == "egg", prior_vacs %in% c(0, 3, 5))
)

seroconv_plot2_2021_egg_035_vac <- fun_seroconv_plot2(
  seroconv_summary %>% filter(vaccinated, year == 2021, virus_egg_cell == "egg", prior_vacs %in% c(0, 3, 5))
)

ggsave(
  "data-summary/seroconv2-2020-egg-015-vac.pdf", 
  seroconv_plot2_2020_egg_015_vac, width = 15, height = 10, units = "cm"
)

ggsave(
  "data-summary/seroconv2-2021-egg-015-vac.pdf", 
  seroconv_plot2_2021_egg_015_vac, width = 15, height = 10, units = "cm"
)

ggsave(
  "data-summary/seroconv2-2020-egg-035-vac.pdf", 
  seroconv_plot2_2020_egg_035_vac, width = 15, height = 10, units = "cm"
)

ggsave(
  "data-summary/seroconv2-2021-egg-035-vac.pdf", 
  seroconv_plot2_2021_egg_035_vac, width = 15, height = 10, units = "cm"
)
