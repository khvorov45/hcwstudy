library(tidyverse)
library(kableExtra)

vac_dates <- read_csv("data/vaccinations.csv", col_types = cols()) %>% filter(!is.na(vaccination_date))
bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())
swabs <- read_csv("data/swabs.csv", col_types = cols())

flupos_dates <- swabs %>%
    filter(swab_result == 1, str_starts(swab_virus, "Flu")) %>%
    select(pid, year, samp_date) %>%
    distinct() %>%
    # NOTE(sen) Only look at the first infection per year
    group_by(pid, year) %>%
    filter(samp_date == min(samp_date)) %>%
    ungroup()

all_dates <- bleed_dates %>%
    filter(samp_type == "serum") %>%
    select(-samp_type) %>%
    mutate(day = as.character(day)) %>%
    bind_rows(vac_dates %>% select(pid, year, date = vaccination_date) %>% mutate(day = "vax")) %>%
    bind_rows(flupos_dates %>% select(pid, year, date = samp_date) %>% mutate(day = "flupos")) %>%
    mutate(day = factor(day, c("0", "vax", "7", "14", "flupos", "220"))) %>%
    arrange(pid, year, day) %>%
    left_join(read_csv("data/participants.csv", col_types = cols()) %>% select(pid, site), "pid") %>%
    arrange(pid)

bleed_intervals <- all_dates %>%
    pivot_wider(names_from = "day", values_from = "date") %>%
    mutate(
        i0_vax = `vax` - `0`,
        i0_flupos = `flupos` - `0`,
        ivax_7 = `7` - `vax`,
        ivax_14 = `14` - `vax`,
        ivax_220 = `220` - `vax`,
        ivax_flupos = `flupos` - `vax`,
    )

bleed_intervals %>%
    filter(i0_vax < -1) %>%
    arrange(i0_vax) %>%
    select(pid, site, year, `0`, `vax`, i0_vax) %>%
    rename(baseline_bleed = `0`) %>%
    arrange(i0_vax, pid, year) %>%
    group_by(site) %>%
    group_walk(~print(.x, n = 100))

bleed_intervals %>%
    filter(ivax_14 < -1) %>%
    arrange(ivax_14) %>%
    select(pid, site, year, `vax`, `14`, ivax_14) %>%
    arrange(ivax_14, pid, year) %>%
    group_by(site) %>%
    group_walk(~print(.x, n = 100))

all_dates %>%
    filter(year == 2021, day == "0") %>%
    mutate(month = lubridate::month(date)) %>%
    arrange(desc(month))

bleed_dates_plots <- all_dates %>%
    filter(!day %in% c("7", "flupos")) %>%
    left_join(bleed_intervals, c("pid", "year", "site")) %>%
    group_by(year) %>%
    group_split() %>%
    map(function(data) {
        this_year <- unique(data$year)
        date_min <- lubridate::ymd(glue::glue("{this_year}-03-01"))
        date_max <- lubridate::ymd(glue::glue("{this_year + 1}-01-01"))
        pl <- data %>%
            mutate(
                pid = fct_reorder(fct_drop(pid), replace_na(as.integer(`0`), 0), .desc = TRUE),
            ) %>%
            ggplot(aes(date, pid, color = day, shape = day)) +
            theme_bw() +
            theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                strip.background = element_blank(),
                panel.spacing = unit(0, "null"),
            ) +
            coord_cartesian(xlim = c(date_min, date_max)) +
            facet_wrap(~year, strip.position = "right") +
            scale_y_discrete(expand = expansion(0.05, 0)) +
            scale_x_date("", breaks = lubridate::ymd(paste0(this_year, "-", 1:12, "-", 1)), labels = function(breaks) {
                month.abb[lubridate::month(breaks)]
            }) +
            scale_color_manual("", values = viridis::viridis_pal(option = "C", end = 0.8)(4)) +
            scale_shape_manual("", values = c(19, 3, 15, 17)) +
            geom_point() +
            geom_point(data = . %>% filter(day == "vax"))
        if (this_year != max(bleed_dates$year)) {
            pl <- pl + theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
            )
        }
        pl
    })

bleed_dates_plot <- ggpubr::ggarrange(plotlist = bleed_dates_plots, common.legend = TRUE, ncol = 1)

save2 <- function(name, ...) {ggsave(paste0(name, ".pdf"), ...);ggsave(paste0(name, ".png"), ...)}

save2(
    "report/figure-bleed-dates/figure-bleed-dates", bleed_dates_plot, width = 15, height = 20, units = "cm"
)

bleed_intervals_plots <- bleed_intervals %>%
    select(pid, site, year, starts_with("i")) %>%
    pivot_longer(starts_with("i"), names_to = "timepoint", values_to = "interval") %>%
    filter(!is.na(interval)) %>%
    mutate(
        interval = as.integer(interval),
        timepoint = factor(timepoint, c("i0_vax", "ivax_7", "ivax_14", "ivax_220", "i0_flupos", "ivax_flupos"))
    ) %>%
    bind_rows(mutate(., site = "total")) %>%
    mutate(plot_group = case_when(
        timepoint == "ivax_220" ~ "1", 
        timepoint %in% c("i0_flupos", "ivax_flupos") ~ "2", 
        TRUE ~ "3",
    )) %>%
    group_by(plot_group) %>%
    group_split() %>%
    map(function(data_subset) {
        ggplot(data_subset, aes(timepoint, interval)) +
        theme_bw() +
        theme(
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.spacing = unit(0, "null"),
            axis.text.x = element_text(angle = 30, hjust = 1)
        ) +
        facet_grid(site~year, scales = "free_x") +
        geom_jitter(shape = 18, width = 0.2, alpha = 0.3, color = "gray50") +
        geom_boxplot(color = "blue", fill = NA, outlier.alpha = 0)
    })

save2(
    "report/figure-bleed-dates/figure-bleed-intervals1",
    bleed_intervals_plots[[1]] +
        scale_x_discrete("Interval", labels = c("vax to 220")) +
        scale_y_continuous("Days", breaks = c(60, 100, 140, 180)),
    width = 12, height = 15, units = "cm"
)

save2(
    "report/figure-bleed-dates/figure-bleed-intervals2",
    bleed_intervals_plots[[2]] +
        scale_x_discrete("Interval", labels = c("0 to flupos", "vax to flupos")) +
        scale_y_continuous("Days", breaks = c(0, 30, 60, 90, 120, 150, 180, 220)),
    width = 12, height = 15, units = "cm"
)

save2(
    "report/figure-bleed-dates/figure-bleed-intervals3",
    bleed_intervals_plots[[3]] +
        coord_cartesian(ylim = c(-10, 40)) +
        scale_x_discrete("Interval", labels = c("0 to vax", "vax to 7", "vax to 14")) +
        scale_y_continuous("Days", breaks = c(0, 14, 30, 60)),
    width = 12, height = 15, units = "cm"
)

covid_vax <- read_csv("data/covid-vax.csv", col_types = cols())
covid_bleeds <- read_csv("data/covid-bleed-dates.csv", col_types = cols())

covid_vax %>%
    select(pid, dose, date) %>%
    pivot_wider(names_from = "dose", values_from = "date") %>%
    filter(`2` < `1` | `3` < `1` | `4` < `1` | `3` < `2` | `4` < `2` | `4` < `3`)

covid_vax_dosedates <- covid_vax %>%
    select(pid, dose, date) %>%
    mutate(dose = paste0("dosedate", dose)) %>%
    pivot_wider(names_from = "dose", values_from = "date")

covid_bleeds %>%
    left_join(covid_vax_dosedates) %>%
    filter(day > 0, date < dosedate1)

covid_vax_plots <- covid_vax %>%
    mutate(date_type = paste0("Dose ", dose)) %>%
    select(pid, date_type, date) %>%
    bind_rows(covid_bleeds %>% filter(day != 0) %>% mutate(date_type = "Bleed (post-vax)") %>% select(pid, date_type, date)) %>%
    left_join(covid_vax_dosedates, "pid") %>%
    filter(!is.na(date)) %>%
    mutate(
        date_type = factor(date_type, c("Dose 1", "Dose 2", "Bleed (post-vax)", "Dose 3", "Dose 4", "Dose 5")),
        dosedate2 = replace_na(dosedate2, lubridate::date("2000-01-01")),
        dosedate1 = replace_na(dosedate1, lubridate::date("2000-01-01")),
    ) %>%
    left_join(covid_vax %>% filter(dose == 2) %>% select(pid, dose2brand = brand), "pid") %>%
    filter(dose2brand %in% c("Astra-Zeneca", "Pfizer"), date_type != "Dose 5") %>%
    group_by(dose2brand) %>%
    group_map(.keep = TRUE, function(onebrand, key) {
        addtheme <- theme()
        if (key$dose2brand == "Astra-Zeneca") {
            addtheme <- theme(
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.length = unit(0, "null")
            )
        }
        onebrand %>%
            mutate(
                pid = fct_reorder(pid, dosedate2, .desc = TRUE) %>% fct_reorder(dosedate1, .desc = TRUE),
                dose2brand = paste0("Dose 2: ", dose2brand),
            ) %>%
            ggplot(aes(date, pid, color = date_type, shape = date_type)) +
            theme_bw() +
            theme(
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.title.y = element_blank(),
                axis.title.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                legend.position = "top",
                axis.text.x = element_text(angle = 30, hjust = 1),
                plot.margin = margin(0, 0, 0, 25),
                strip.background = element_blank(),
            ) +
            coord_cartesian(xlim = lubridate::ymd(c("2021-01-01", "2023-01-01"))) +
            facet_wrap(~dose2brand, strip.position = "right") +
            scale_color_manual("", values = c("gray50", "blue", "red", "gray50", "gray50")) +
            scale_shape_manual("", values = c(20, 17, 3,  4, 1)) +
            scale_y_discrete(expand = expansion(0.05, 0)) +
            scale_x_date(breaks = "2 month") +
            geom_point() +
            addtheme
    })

covid_vax_plot <- ggpubr::ggarrange(
    plotlist = covid_vax_plots,
    ncol = 1, common.legend = TRUE,
    align = "v", heights = c(0.85, 1)
)

save2(
    "report/figure-bleed-dates/figure-covid-bleed-dates", covid_vax_plot, width = 15, height = 20, units = "cm"
)

covid_vax %>%
    select(pid, dose, brand) %>%
    pivot_wider(names_from = "dose", values_from = "brand") %>%
    count(`1`, `2`, `3`, `4`) %>%
    arrange(desc(n)) %>%
    select(`Dose 1` = `1`, `Dose 2` = `2`, `Dose 3` = `3`, `Dose 4` = `4`, Subjects = n) %>%
    mutate(across(starts_with("Dose "), ~replace_na(.x, "None recorded"))) %>%
    write_csv("report/figure-bleed-dates/covax-brands.csv") %>%
    kbl(
        format = "latex",
        caption = "Counts of subjects for each covid vaccine brand combination.",
        booktabs = TRUE,
        label = "covax-brands",
    ) %>%
    str_replace_all("None recorded", "\\\\textcolor{gray}{None recorded}") %>%
    write("report/figure-bleed-dates/covax-brands.tex")
