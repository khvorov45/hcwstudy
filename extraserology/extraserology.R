library(tidyverse)

vac_hist <- read_csv("./data/vaccinations.csv", col_types = cols())

prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
        prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
    )

vax2020_pids <- vac_hist %>% filter(year == 2020, (status == "Australia" | status == "Overseas")) %>% pull(pid)
vax2021_pids <- vac_hist %>% filter(year == 2021, (status == "Australia" | status == "Overseas")) %>% pull(pid)

# TODO(sen) Probably have prior_vac_counts as a separate table
titres <- read_csv("data/serology.csv", col_types = cols()) %>%
    filter(subtype == "H1", year %in% 2020:2021, virus_egg_cell == "egg", day %in% c(0, 14, 220)) %>%
    left_join(read_csv("data/participants.csv", col_types = cols()), "pid") %>%
    left_join(prior_vac_counts, "pid") %>%
    mutate(
        prior_vax_in_serum_year = if_else(year == 2020, prior2020, prior2021),
        prior_vax_in_serum_year_cat = case_when(
            prior_vax_in_serum_year == 0 ~ "0",
            prior_vax_in_serum_year %in% 1:2 ~ "1-2",
            prior_vax_in_serum_year %in% 3:5 ~ "3-5",
        ),
        titre_type = if_else(
            (year == 2020 & virus == "A/Brisbane/02/2018e") | (year == 2021 & virus == "A/Victoria/2570/2019e"),
            "vaccine", "other" 
        ) %>% 
            factor(c("vaccine", "other")),
    ) %>%
    group_by(pid, year) %>%
    # NOTE(sen) Limit to the vaccinated (have postvax bleed) in at least one year
    filter(14 %in% day) %>%
    group_by(pid) %>%
    mutate(group = case_when(
        14 %in% day[year == 2020] & 14 %in% day[year == 2021] ~ "vax2020and2021",
        14 %in% day[year == 2020] ~ "vax2020",
        14 %in% day[year == 2021] ~ "vax2021",
        TRUE ~ NA_character_
    )) %>%
    ungroup()

# NOTE(sen) Find everyone without extra serology
more_serology <- read_csv("data/serology.csv", col_types = cols()) %>%
    group_by(pid) %>%
    mutate(
        anybleed2020 = 2020 %in% year, 
        anybleed2021 = 2021 %in% year,
        yes2020d14_yes2021d14 = (14 %in% day[year == 2020]) &  (14 %in% day[year == 2021]),
        yes2020d14_no2021d14 =  (14 %in% day[year == 2020]) & !(14 %in% day[year == 2021]),
        no2020d14_yes2021d14 = !(14 %in% day[year == 2020]) &  (14 %in% day[year == 2021]),
        no2020d14_no2021d14 =  !(14 %in% day[year == 2020]) & !(14 %in% day[year == 2021]),
    ) %>%
    ungroup() %>%
    filter(subtype == "H1", year %in% 2020:2021, virus_egg_cell == "egg", day %in% c(0, 14)) %>%
    group_by(pid) %>%
    mutate(
        yes2020Victoria = any(str_detect(virus[year == 2020], "Victoria")),
        yes2021Brisbane = any(str_detect(virus[year == 2021], "Brisbane")),
        redcap2020vax = pid %in% vax2020_pids,
        redcap2021vax = pid %in% vax2021_pids,
    ) %>%
    ungroup() %>%
    select(pid, matches("(yes)|(no)|(redcap)|(anybleed)")) %>%
    distinct() %>%
    mutate(across(-pid, as.integer))

more_serology %>% count(no2020d14_yes2021d14, redcap2020vax)

write_csv(more_serology, "extraserology/titres_tested.csv")

titres_plot <- titres %>%
    mutate(
        day = if_else(day == 220, 50, day), 
        year_color = if_else(year == 2020, "#008080", "#ff00ff"),
        year_shade = if_else(year == 2020, "gray70", "black"),
    ) %>%
    ggplot(aes(day, titre, color = year_color)) +
    theme_bw() +
    theme(
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "null"),
        strip.background = element_blank(),
    ) +
    scale_y_log10("Titre", breaks = 5 * 2^(0:15)) +
    scale_x_continuous("Day", breaks = c(0, 14, 50), labels = c(0, 14, 220)) +
    facet_grid(titre_type ~ year) +
    geom_line(aes(group = pid), alpha = 0.05) +
    geom_point(alpha = 0.05, shape = 18)  + 
    scale_color_identity() +
    geom_boxplot(aes(group = paste0(day, year, virus), color = year_shade), alpha = 0.5, fill = NA, outlier.alpha = 0, size = 1) +
    geom_boxplot(aes(group = paste0(day, year, virus), color = year_color), fill = NA, outlier.alpha = 0) +
    geom_text(
        aes(20, 1, label = virus), 
        data = titres %>% select(year, titre_type, virus) %>% distinct(),
        color = "gray10",
        size = 3
    )

ggsave("extraserology/titres_plot.pdf", titres_plot, width = 12, height = 12, units = "cm")
ggsave("extraserology/titres_plot.jpg", titres_plot, width = 12, height = 12, units = "cm")

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
  f <- function(x) round(x, round_to)
  string <- glue::glue("{f(mean)} ({f(low)}, {f(high)})")
  tibble(mean, low, high, string)
}

gmts_homologous_pre_post <- titres %>% 
    filter(day %in% c(0, 14), titre_type == "vaccine") %>%
    group_by(year, day, prior_vax_in_serum_year_cat) %>% 
    summarise(.groups = "drop", summarise_logmean(titre)) %>%
    mutate(timepoint = recode(as.factor(day), "0" = "Pre-vax", "14" = "Post-vax"))

make_gmt_plot <- function(data, filename, width) {
    data %>%
        mutate(year_color = if_else(year == 2020, "#008080", "#ff00ff")) %>%
        ggplot(aes(timepoint, mean, ymin = low, ymax = high)) +
        theme_bw() +
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position = "bottom",
            strip.background = element_blank(),
            axis.title.x = element_blank(),
            strip.placement = "outside",
            panel.spacing = unit(0, "null"),
        ) +
        facet_wrap(~year, strip.position = "bottom") +
        scale_x_discrete("", expand = expansion(0, 0)) +
        scale_y_log10("Geometric mean titre", breaks = 5 * 2^(0:15)) +
        scale_shape_manual("Prior vaccinations", values = c(1, 8, 0)) +
        coord_cartesian(ylim = c(5, 1280), xlim = c(0.5, 2.5)) +
        scale_color_identity() +
        geom_pointrange(
            aes(shape = prior_vax_in_serum_year_cat, color = year_color), 
            position = position_dodge(width = 0.5)
        ) +
        geom_vline(xintercept = 1.5, color = "gray50")
    ggsave(glue::glue("{filename}.jpg"), width = width, height = 10, units = "cm")
    ggsave(glue::glue("{filename}.pdf"), width = width, height = 10, units = "cm")
}

make_gmt_plot(gmts_homologous_pre_post %>% filter(year == 2020), "extraserology/gmt_plot_h1_homologous2020", 10)
make_gmt_plot(gmts_homologous_pre_post %>% filter(year == 2021), "extraserology/gmt_plot_h1_homologous2021", 10)
make_gmt_plot(gmts_homologous_pre_post, "extraserology/gmt_plot_h1_homologous2020_2021", 15)

ratios <- titres %>%
    pivot_wider(names_from = "day", values_from = "titre") %>%
    mutate(ratio = `14` / `0`, seroconv = as.integer(ratio >= 4))

gmr_homologous_pre_post <- ratios %>%
    filter(titre_type == "vaccine") %>%
    group_by(prior_vax_in_serum_year_cat, year) %>%
    summarise(.groups = "drop", summarise_logmean(ratio))

gmr_homologous_pre_post_plot <- gmr_homologous_pre_post %>%
    mutate(year_color = if_else(year == 2020, "#008080", "#ff00ff")) %>%
    ggplot(aes(as.factor(year), mean, ymin = low, ymax = high)) +
    theme_bw() +
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(0, "null"),
    ) +
    scale_y_log10("Geometric mean rise", breaks = c(2^(0:15))) +
    scale_shape_manual("Prior vaccinations", values = c(1, 8, 0)) +
    scale_color_identity() +
    coord_cartesian(ylim = c(1, 128)) +
    geom_pointrange(
        aes(shape = prior_vax_in_serum_year_cat, color = year_color), 
        position = position_dodge(width = 0.5)
    )

ggsave(glue::glue("extraserology/gmr_plot_h1_homologous.pdf"), gmr_homologous_pre_post_plot, width = 10, height = 10, units = "cm")
ggsave(glue::glue("extraserology/gmr_plot_h1_homologous.jpg"), gmr_homologous_pre_post_plot, width = 10, height = 10, units = "cm")

summarise_prop <- function(vec) {
    vec <- na.omit(vec)
    success <- sum(vec)
    total <- length(vec)
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

seroconv_homologous_pre_post <- ratios %>%
    filter(titre_type == "vaccine") %>%
    group_by(prior_vax_in_serum_year_cat, year) %>%
    summarise(.groups = "drop", summarise_prop(seroconv))

seroconv_homologous_pre_post <- seroconv_homologous_pre_post %>%
    mutate(year_color = if_else(year == 2020, "#008080", "#ff00ff")) %>%
    ggplot(aes(as.factor(year), prop, ymin = low, ymax = high)) +
    theme_bw() +
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(0, "null"),
    ) +
    scale_y_continuous("Proportion serconverted", breaks = seq(0, 1, 0.1)) +
    scale_shape_manual("Prior vaccinations", values = c(1, 8, 0)) +
    scale_color_identity() +
    coord_cartesian(ylim = c(0, 1)) +
    geom_pointrange(
        aes(shape = prior_vax_in_serum_year_cat, color = year_color), 
        position = position_dodge(width = 0.5)
    )

ggsave(glue::glue("extraserology/seroconv_plot_h1_homologous.pdf"), seroconv_homologous_pre_post, width = 10, height = 10, units = "cm")
ggsave(glue::glue("extraserology/seroconv_plot_h1_homologous.jpg"), seroconv_homologous_pre_post, width = 10, height = 10, units = "cm")

ratios_fit <- lm(I(log(ratio)) ~ as.factor(year) * titre_type, ratios)
ratios_coefs <- coef(ratios_fit)
ratios_vcov <- vcov(ratios_fit)

calc_ratios <- function(names, year, type) {
    tibble(
        logrise = sum(ratios_coefs[names]), 
        variance = sum(ratios_vcov[names, names]),
        year = year, type = type,
    )
}

fit_result <- tibble(calc_ratios("(Intercept)", 2020, "vaccine")) %>%
    bind_rows(calc_ratios(c("(Intercept)", "titre_typeother"), 2020, "other")) %>%
    bind_rows(calc_ratios(c("(Intercept)", "as.factor(year)2021"), 2021, "vaccine")) %>%
    bind_rows(calc_ratios(c("(Intercept)", "titre_typeother", "as.factor(year)2021", "as.factor(year)2021:titre_typeother"), 2021, "other")) %>%
    mutate(
        se = sqrt(variance),
        logriselow = logrise - 1.96 * se,
        logrisehigh = logrise + 1.96 * se,
        rise = exp(logrise),
        riselow = exp(logriselow),
        risehigh = exp(logrisehigh),
        type = factor(type, c("vaccine", "other")),
    )

write_csv(fit_result, "extraserology/vaccine_response_fit.csv")

fit_plot <- fit_result %>%
    ggplot(aes(type, rise, ymin = riselow, ymax = risehigh)) +
    theme_bw() +
    theme(
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(0, "null"),
        axis.title.x = element_blank(),
    ) +
    scale_y_continuous("Rise", breaks = 0:15) +
    facet_wrap(~year) +
    geom_pointrange()

ggsave("extraserology/vaccine_response_plot.pdf", fit_plot, width = 7, height = 10, units = "cm")
