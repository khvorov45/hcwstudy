library(tidyverse)
library(kableExtra)

vac_hist <- read_csv("data/vaccinations.csv", col_types = cols())

prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        .groups = "drop",
        prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
        prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
        prior2023 = sum(year >= 2018 & year < 2023 & (status == "Australia" | status == "Overseas")),
    )

participants <- read_csv("data/participants.csv", col_types = cols()) %>%
    left_join(prior_vac_counts, "pid") %>%
    mutate(
        prior_at_recruitment = case_when(
            recruitment_year == 2020 ~ prior2020,
            recruitment_year == 2021 ~ prior2021,
            recruitment_year == 2022 ~ prior2022,
            recruitment_year == 2023 ~ prior2023,
        )
    )

participants_filtered_age <- participants %>%
    filter(!is.na(age_screening), age_screening >= 18, age_screening <= 65)

participants_age_priorvac_plot <- participants_filtered_age %>%
    mutate(recruitment_year = as.character(recruitment_year)) %>%
    bind_rows(participants_filtered_age %>% mutate(recruitment_year = "Combined")) %>%
    mutate(
        recruitment_year = factor(recruitment_year, c("2020", "2021", "2022", "2023", "Combined")),
        xpos = prior_at_recruitment + (as.integer(recruitment_year) - 3) * 0.15
    ) %>%
    ggplot(aes(xpos, age_screening, col = factor(recruitment_year))) +
    theme_bw() +
    scale_x_continuous("Known prior vaccinations in 5 years before recruitment", breaks = 0:5) +
    scale_y_continuous("Age") +
    scale_color_viridis_d("Recrutment year", begin = 0.2, end = 0.8, option = "A") +
    geom_boxplot(aes(group = paste0(prior_at_recruitment, recruitment_year)))

participants_with_bmi <- participants %>%
    filter(height > 100, height < 250, weight > 20, weight < 400) %>%
    mutate(bmi = weight / (height / 100)^2)

participants_bmi_priorvac_plot <- participants_with_bmi %>%
    mutate(recruitment_year = as.character(recruitment_year)) %>%
    bind_rows(participants_with_bmi %>% mutate(recruitment_year = "Combined")) %>%
    mutate(
        recruitment_year = factor(recruitment_year, c("2020", "2021", "2022", "2023", "Combined")),
        xpos = prior_at_recruitment + (as.integer(recruitment_year) - 3) * 0.15
    ) %>%
    ggplot(aes(xpos, bmi, col = factor(recruitment_year))) +
    theme_bw() +
    scale_x_continuous("Known prior vaccinations in 5 years before recruitment", breaks = 0:5) +
    scale_y_continuous("BMI") +
    scale_color_viridis_d("Recrutment year", begin = 0.2, end = 0.8, option = "A") +
    geom_boxplot(aes(group = paste0(prior_at_recruitment, recruitment_year)))

baseline_plots <- ggpubr::ggarrange(
    plotlist = list(
        participants_age_priorvac_plot,
        participants_bmi_priorvac_plot
    ),
    ncol = 1, common.legend = TRUE
)

(function(name, ...) {ggsave(paste0(name, ".pdf"), ...);ggsave(paste0(name, ".png"), ...)})(
    "report/baseline/baseline", baseline_plots, width = 15, height = 15, units = "cm"
)

boxplotsummarystr <- function(x) {
    f <- function(x) round(x, 0)
    glue::glue(
        "{f(median(x))} [{f(quantile(x, 0.25))}, {f(quantile(x, 0.75))}]",
    )
}

sumgender <- function(data) {
    sums <- data %>%
        mutate(gender = replace_na(gender, "missing")) %>%
        count(gender) %>%
        mutate(gender = recode(gender, "female" = "f", "male" = "m", "other" = "o", "missing" = "n") %>% factor(c("f", "m", "o", "n"))) %>%
        arrange(gender) %>%
        mutate(str = paste0(n, "", gender))
    tibble(genderstr = paste(sums$str, collapse = " "))
}

arrange_by_year <- function(data) {
    data %>% mutate(year = factor(year, c("2020", "2021", "2022", "2023", "Combined"))) %>% arrange(year)
}

participants %>%
    count(prior_at_recruitment) %>%
    mutate(prior_at_recruitment = as.character(prior_at_recruitment)) %>%
    bind_rows(participants %>% count() %>% mutate(prior_at_recruitment = "Combined")) %>%
    mutate(year = "Combined") %>%
    bind_rows(participants %>%
        count(recruitment_year) %>%
        mutate(prior_at_recruitment = "Combined", year = as.character(recruitment_year)) %>%
        select(-recruitment_year)
    ) %>% bind_rows(participants %>%
        count(recruitment_year, prior_at_recruitment) %>%
        mutate(prior_at_recruitment = as.character(prior_at_recruitment), year = as.character(recruitment_year)) %>%
        select(-recruitment_year)
    ) %>%
    mutate(n = as.character(n)) %>%
    pivot_wider(names_from = "prior_at_recruitment", values_from = "n") %>%
    mutate(var = "Count") %>%
    arrange_by_year() %>%
    bind_rows(participants_filtered_age %>%
        group_by(prior_at_recruitment) %>%
        summarise(age_str = boxplotsummarystr(age_screening)) %>%
        mutate(prior_at_recruitment = as.character(prior_at_recruitment), year = "Combined") %>%
        bind_rows(summarise(participants_filtered_age, age_str = boxplotsummarystr(age_screening), prior_at_recruitment = "Combined", year = "Combined")) %>%
        bind_rows(participants_filtered_age %>%
            group_by(prior_at_recruitment, recruitment_year) %>%
            summarise(age_str = boxplotsummarystr(age_screening), .groups = "drop") %>%
            mutate(recruitment_year = as.character(recruitment_year), prior_at_recruitment = as.character(prior_at_recruitment)) %>%
            rename(year = recruitment_year)
        ) %>% bind_rows(participants_filtered_age %>%
            group_by(recruitment_year) %>%
            summarise(age_str = boxplotsummarystr(age_screening), prior_at_recruitment = "Combined") %>%
            mutate(recruitment_year = as.character(recruitment_year)) %>%
            rename(year = recruitment_year)
        ) %>%
        pivot_wider(names_from = "prior_at_recruitment", values_from = age_str) %>%
        mutate(var = "Age (years) at recruitment") %>%
        arrange_by_year()
    ) %>%
    bind_rows(participants %>%
        group_by(prior_at_recruitment) %>%
        group_modify(~sumgender(.x)) %>%
        mutate(prior_at_recruitment = as.character(prior_at_recruitment), year = "Combined") %>%
        bind_rows(sumgender(participants) %>% mutate(prior_at_recruitment = "Combined", year = "Combined")) %>%
        bind_rows(
            participants %>%
                group_by(recruitment_year) %>%
                group_modify(~sumgender(.x)) %>%
                mutate(prior_at_recruitment = "Combined", recruitment_year = as.character(recruitment_year)) %>%
                rename(year = recruitment_year)
        ) %>%
        bind_rows(
            participants %>%
                group_by(recruitment_year, prior_at_recruitment) %>%
                group_modify(~sumgender(.x)) %>%
                mutate(prior_at_recruitment = as.character(prior_at_recruitment), recruitment_year = as.character(recruitment_year)) %>%
                rename(year = recruitment_year)
        ) %>%
        pivot_wider(names_from = "prior_at_recruitment", values_from = "genderstr") %>%
        mutate(var = "Gender") %>%
        arrange_by_year()
    ) %>%
    bind_rows(participants_with_bmi %>%
        group_by(prior_at_recruitment) %>%
        summarise(bmi_str = boxplotsummarystr(bmi)) %>%
        mutate(prior_at_recruitment = as.character(prior_at_recruitment), year = "Combined") %>%
        bind_rows(summarise(participants_with_bmi, bmi_str = boxplotsummarystr(bmi), prior_at_recruitment = "Combined", year = "Combined")) %>%
        bind_rows(
            participants_with_bmi %>%
                group_by(prior_at_recruitment, recruitment_year) %>%
                summarise(bmi_str = boxplotsummarystr(bmi), .groups = "drop") %>%
                mutate(prior_at_recruitment = as.character(prior_at_recruitment), recruitment_year = as.character(recruitment_year)) %>%
                rename(year = recruitment_year)
        ) %>%
        bind_rows(
            participants_with_bmi %>%
                group_by(recruitment_year) %>%
                summarise(bmi_str = boxplotsummarystr(bmi), .groups = "drop") %>%
                mutate(prior_at_recruitment = "Combined", recruitment_year = as.character(recruitment_year)) %>%
                rename(year = recruitment_year)
        ) %>%
        pivot_wider(names_from = "prior_at_recruitment", values_from = bmi_str) %>%
        mutate(var = "BMI") %>%
        arrange_by_year()
    ) %>%
    select(var, everything()) %>%
    write_csv("report/baseline/baseline.csv") %>%
    kbl(
        format = "latex",
        caption = "Baseline characteristics. Format: median [lower quartile, upper quartile].
        Gender key: m - male, f - female, o - other, n - not recorded.",
        booktabs = TRUE,
        label = "baseline",
        col.names = c("", "Year", colnames(.)[3:ncol(.)]),
    ) %>%
    add_header_above(c(" " = 2, "Known vaccinations in the 5 years before recruitment" = 6)) %>%
    column_spec(1, width = "2cm") %>%
    column_spec(3:9, width = "2.5cm") %>%
    collapse_rows(columns = 1, latex_hline = "major") %>%
    kable_styling(latex_options = "scale_down") %>%
    write("report/baseline/baseline.tex")
