library(tidyverse)

vac_dates <- read_csv("data/vaccinations.csv", col_types = cols()) %>% filter(!is.na(vaccination_date))
bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())

all_dates <- bleed_dates %>%
    mutate(day = as.character(day)) %>%
    bind_rows(vac_dates %>% select(pid, year, date = vaccination_date) %>% mutate(day = "vax")) %>%
    mutate(day = factor(day, c("0", "vax", "7", "14", "220"))) %>%
    arrange(pid, year, day) %>%
    left_join(read_csv("data/participants.csv", col_types = cols()) %>% select(pid, site), "pid")

bleed_intervals <- all_dates %>%
    pivot_wider(names_from = "day", values_from = "date") %>%
    mutate(
        i0_vax = `vax` - `0`,
        ivax_7 = `7` - `vax`,
        ivax_14 = `14` - `vax`,
        ivax_220 = `220` - `vax`,
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

bleed_dates_plots <- all_dates %>%
    left_join(bleed_intervals, c("pid", "year")) %>%
    group_by(year) %>%
    group_split() %>%
    map(function(data) {
        this_year <- unique(data$year)
        date_min <- lubridate::ymd(glue::glue("{this_year}-03-01"))
        date_max <- lubridate::ymd(glue::glue("{this_year + 1}-01-01"))
        pl <- data %>%
            mutate(
                pid = fct_reorder(pid, `0`, .desc = TRUE),
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
            scale_color_manual(values = viridis::viridis_pal(option = "A", end = 0.8)(5)) +
            scale_shape_manual(values = c(19, 3, 4, 17, 18)) +
            geom_point()
        if (this_year != max(bleed_dates$year)) {
            pl <- pl + theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
            )
        }
        pl
    })

bleed_dates_plot <- ggpubr::ggarrange(plotlist = bleed_dates_plots, common.legend = TRUE, ncol = 1)

ggsave("report/figure-bleed-dates/figure-bleed-dates.pdf", bleed_dates_plot, width = 15, height = 20, units = "cm")

bleed_intervals_plot <- bleed_intervals %>%
    select(pid, year, starts_with("i")) %>%
    pivot_longer(starts_with("i"), names_to = "timepoint", values_to = "interval") %>%
    filter(!is.na(interval)) %>%
    mutate(
        interval = as.integer(interval),
        timepoint = factor(timepoint, c("i0_vax", "ivax_7", "ivax_14", "ivax_220"))
    ) %>%
    ggplot(aes(timepoint, interval)) +
    theme_bw() +
    theme(
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(0, "null"),
        axis.text.x = element_text(angle = 30, hjust = 1)
    ) +
    facet_wrap(~year, nrow = 1, strip.position = "top") +
    scale_y_continuous("Days", breaks = c(0, 7, 14, 30, 60, 90, 120, 150, 180, 220)) +
    scale_x_discrete("Interval", labels = c("0 to vax", "vax to 7", "vax to 14", "vax to 220")) +
    geom_jitter(shape = 18, width = 0.2, alpha = 0.3, color = "gray50") +
    geom_boxplot(color = "blue", fill = NA, outlier.alpha = 0)

ggsave("report/figure-bleed-dates/figure-bleed-intervals.pdf", bleed_intervals_plot, width = 15, height = 15, units = "cm")
