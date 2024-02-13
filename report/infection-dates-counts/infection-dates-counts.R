library(tidyverse)
library(kableExtra)

swabs <- read_csv("data/swabs.csv", col_types = cols())
postinf_bleed_dates <- read_csv("data/postinf-bleed-dates.csv", col_types = cols())

all_dates <- swabs %>%
    filter(swab_result == 1, swab_virus != "Negative") %>%
    group_by(pid, year, postinf_instance, samp_date) %>%
    summarise(swab_viruses = paste(swab_virus, collapse = ", "), .groups = "drop") %>%
    mutate(swab_viruses = recode(swab_viruses, 
        "Flu A (unsubtyped), Flu A H1" = "Flu A H1",
        "Flu A (unsubtyped), Flu A H3" = "Flu A H3",
    )) %>%
    select(pid, year, postinf_instance, date = samp_date, date_type = swab_viruses) %>%
    bind_rows(
        postinf_bleed_dates %>%
            mutate(date_type = as.character(day)) %>%
            select(pid, year, postinf_instance, date = bleed_date, date_type),
    ) %>%
    filter(!is.na(date)) %>%
    mutate(
        date_cat = case_when(
            date_type == "7" ~ "Bleed",
            date_type == "14" ~ "Bleed",
            date_type == "30" ~ "Bleed",
            !str_detect(tolower(date_type), "parainfluenza") & str_detect(tolower(date_type), "flu") ~ "Swab (flu pos)",
            str_detect(tolower(date_type), "sars-cov-2") ~ "Swab (covid pos)",
            TRUE ~ "Swab (other pos)",
        ) %>% factor(c("Swab (flu pos)", "Swab (covid pos)", "Swab (other pos)", "Bleed"))
    ) %>%
    # NOTE(sen) The same swab is sometimes recorded twice
    group_by(pid, year, date, date_type, date_cat) %>%
    filter(row_number() == 1) %>%
    ungroup()

dates_plots <- all_dates %>%
    group_by(year) %>%
    group_split() %>%
    map(function(yeardata) {
        this_year <- unique(yeardata$year)
        xlims <- lubridate::ymd(glue::glue("{this_year}-01-01"), glue::glue("{this_year}-12-31"))
        earliest_dates <- yeardata %>% group_by(pid) %>% summarise(date_earliest = min(date))

        plot <- yeardata %>%
            left_join(earliest_dates, "pid") %>%
            mutate(pid = fct_reorder(pid, date_earliest, .desc = TRUE)) %>%
            ggplot(aes(date, pid, color = date_cat, shape = date_cat)) +
            theme_bw() +
            theme(
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                strip.background = element_blank(),
                axis.title.x = element_blank()
            ) +
            facet_wrap(~year, strip.position = "right") +
            coord_cartesian(xlim = xlims) +
            scale_x_date("", breaks = lubridate::ymd(paste0(this_year, "-", 1:12, "-", 1)), labels = function(breaks) {
                month.abb[lubridate::month(breaks)]
            }) +
            scale_y_discrete(expand = expansion(0.1, 0)) +
            scale_color_manual("", drop = FALSE, values = c("blue", "orange", "gray50", "red")) +
            scale_shape_manual("", drop = FALSE, values = c(19, 15, 1, 3)) +
            geom_point()

        if (this_year < max(all_dates$year)) {
            plot <- plot + theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
            )
        }

        plot
    })

dates_plots_arranged <- ggpubr::ggarrange(plotlist = dates_plots, ncol = 1, common.legend = TRUE)

(function(name, ...) {ggsave(paste0(name, ".pdf"), ...);ggsave(paste0(name, ".png"), ...)})(
    "report/infection-dates-counts/figure-infection-dates",
    dates_plots_arranged, width = 15, height = 20, units = "cm"
)

all_dates_with_total_year <- all_dates %>%
    mutate(year = factor(year)) %>%
    bind_rows(all_dates %>% mutate(year = "Total")) %>%
    mutate(year = fct_relevel(year, "2020", "2021", "2022", "2023", "Total"))

all_table_data <- all_dates_with_total_year %>%
    count(date_type, year) %>%
    bind_rows(
        all_dates_with_total_year %>%
            count(year, date_cat) %>%
            rename(date_type = date_cat) %>%
            mutate(date_type = recode(date_type,
                "Swab (other pos)" = "Total non-flu non-covid",
                "Swab (covid pos)" = "Total SARS-CoV-2",
                "Swab (flu pos)" = "Total flu",
                "Bleed" = "Total bleeds",
            ))
    ) %>%
    bind_rows(all_dates_with_total_year %>%
        filter(!date_type %in% c("7", "14", "30")) %>%
        count(year) %>%
        mutate(date_type = "Total positive")
    ) %>%
    bind_rows(
        all_dates_with_total_year %>%
            group_by(year, date_cat) %>%
            summarise(n = length(unique(pid)), .groups = "drop") %>%
            rename(date_type = date_cat) %>%
            mutate(date_type = recode(date_type,
                "Swab (other pos)" = "Subjects non-flu non-covid",
                "Swab (covid pos)" = "Subjects SARS-CoV-2",
                "Swab (flu pos)" = "Subjects flu",
                "Bleed" = "Subjects bleeds",
            ))
    ) %>%
    bind_rows(all_dates_with_total_year %>%
        filter(!date_type %in% c("7", "14", "30")) %>%
        group_by(year) %>%
        summarise(n = length(unique(pid))) %>%
        mutate(date_type = "Subjects positive")
    ) %>%
    arrange(year) %>%
    pivot_wider(names_from = "year", values_from = "n", values_fill = 0)

all_table_data %>%
    filter(!str_detect(date_type, "bleed"), !date_type %in% c("7", "14", "30")) %>%
    mutate(
        date_type = fct_relevel(date_type, c(
            "Flu A (unsubtyped)", "Flu A H1", "Flu A H3", "Flu B (no lineage)", "SARS-CoV-2",
            "Adenovirus", "Metapneumovirus", "Piconavirus", "Parainfluenza", 
            "Coronavirus OC43, 229E, NL63, HKU, SARS", "Other",
            "Flu A (unsubtyped), Piconavirus",
            "Flu A (unsubtyped), Other",
            "Piconavirus, SARS-CoV-2",
            "SARS-CoV-2, Other",
            "Metapneumovirus, Piconavirus",
            "Parainfluenza, Piconavirus",
            "Piconavirus, Other",
            "Total flu", "Subjects flu", 
            "Total SARS-CoV-2", "Subjects SARS-CoV-2",
            "Total non-flu non-covid", "Subjects non-flu non-covid", 
            "Total positive"
        )) %>%
            recode("Piconavirus, SARS-CoV-2" = "SARS-CoV-2, Piconavirus",)
    ) %>%
    arrange(date_type) %>%
    rename(` ` = date_type) %>%
    write_csv(glue::glue("report/infection-dates-counts/table-infection-counts-swabs.csv")) %>%
    kbl(
        format = "latex",
        caption = "Counts of positive swabs with their test result.
        If a swab had two (or more) positive results it does not count multiple times but instead its
        results are merged and comma-separated (e.g., \"Parainfluenza, Piconavirus\").
        Summary rows with 'Total' count swabs. 
        Summary rows with 'Subjects' count unique participants that contributed the swabs.",
        booktabs = TRUE,
        label = "infection-counts-swabs",
        linesep = c(
            "", "", "", "\\addlinespace", "\\addlinespace", "",
            "", "", "", "", "\\addlinespace", "", "\\addlinespace", "", "\\addlinespace",
            "", "", "\\addlinespace", "", "\\addlinespace",
            "", "\\addlinespace", "", "\\addlinespace", ""
        )
    ) %>%
    write("report/infection-dates-counts/table-infection-counts-swabs.tex")

all_table_data %>%
    filter(str_detect(date_type, "bleed") | date_type %in% c("7", "14", "30")) %>%
    mutate(
        date_type = fct_relevel(date_type, c("7", "14", "30", "Total bleeds", "Subjects bleeds")) %>%
            recode(
                "7" = "Post-infection bleed day 7",
                "14" = "Post-infection bleed day 14",
                "30" = "Post-infection bleed day 30",
            )
    ) %>%
    arrange(date_type) %>%
    rename(` ` = date_type) %>%
    write_csv(glue::glue("report/infection-dates-counts/table-infection-counts-bleeds.csv")) %>%
    kbl(
        format = "latex",
        caption = "Counts of post-infection bleeds",
        booktabs = TRUE,
        label = "infection-counts-bleeds",
        # linesep = c()
    ) %>%
    write("report/infection-dates-counts/table-infection-counts-bleeds.tex")
