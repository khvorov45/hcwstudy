library(tidyverse)

covidser <- read_csv("data/serology-covid.csv", col_types = cols()) %>%
    filter(vax_inf == "V") %>%
    # NOTE(sen) Covax brand
    inner_join(
        read_csv("data/covid-vax.csv", col_types = cols()) %>%
            select(pid, dose, brand) %>%
            filter(dose >= 1, dose <= 2) %>%
            pivot_wider(names_from = "dose", values_from = "brand") %>%
            # NOTE(sen) Keep only those with matching dose1/dose2 brands
            filter(`1` == `2`) %>%
            mutate(brand = `1`) %>%
            select(pid, brand),
        "pid"
    ) %>%
    # NOTE(sen) Baseline (age, bmi)
    inner_join(
        read_csv("data/participants.csv", col_types = cols()) %>%
            filter(
                age_screening >= 18, age_screening <= 65,
                weight >= 30, weight <= 250, 
                height >= 100, height <= 250
            ) %>%
            filter(!is.na(age_screening), !is.na(bmi)) %>%
            mutate(bmi_cat = cut(bmi, c(-Inf, 18.5, 25, 30, Inf))) %>%
            select(pid, age_screening, bmi, bmi_cat),
        "pid"
    ) %>%
    # NOTE(sen) Dose 1/2 interval
    inner_join(
        read_csv("data/covid-vax.csv", col_types = cols()) %>%
            select(pid, dose, date) %>%
            filter(dose >= 1, dose <= 2) %>%
            pivot_wider(names_from = "dose", values_from = "date") %>%
            mutate(dose1_2_days = as.integer(`2` - `1`)) %>%
            filter(dose1_2_days > 0) %>%
            select(pid, dose1_2_days),
        "pid"
    ) %>%
    # NOTE(sen) Dose 2/bleed interval
    inner_join(
        read_csv("data/covid-vax.csv", col_types = cols()) %>%
            filter(dose == 2) %>%
            select(pid, dose2date = date) %>%
            inner_join(
                read_csv("data/covid-bleed-dates.csv", col_types = cols()) %>%
                    filter(day == 14) %>%
                    group_by(pid) %>%
                    # NOTE(sen) Some duplicate data
                    filter(year == max(year)) %>%
                    ungroup() %>%
                    select(pid, d14bleed_date = date),
                "pid"
            ) %>%
            mutate(dose2_d14bleed_days = as.integer(`d14bleed_date` - `dose2date`)) %>%
            filter(dose2_d14bleed_days > 0) %>%
            select(pid, dose2_d14bleed_days),
        "pid"
    ) %>%
    # NOTE(sen) Comorbidities
    left_join(
        read_csv("data/comorbidities.csv", col_types = cols()) %>%
            filter(condition != "Pregnancy") %>%
            mutate(
                condition = recode(condition, 
                    "Diabetes or other metabolic disorder" = "Metabolic disorder",
                    "Chronic respiratory condition" = "Chronic respiratory",
                    "Immunocompromising condition" = "Immunocompromising",
                ),
                condition = paste0("condition_", condition), 
                status = 1
            ) %>%
            pivot_wider(names_from = condition, values_from = "status"),
        "pid"
    ) %>%
    mutate(across(starts_with("condition_"), ~replace_na(.x, 0)))

check_no_rows <- function(data) {
    stopifnot(nrow(data) == 0)
}

# NOTE(sen) Check for duplicates
check_no_rows(covidser %>% group_by(pid, bleed_day_id, strain) %>% filter(n() > 1))

# NOTE(sen) Shouldn't be any missing data
check_no_rows(covidser %>% filter(!complete.cases(covidser)))

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
	tibble(mean, low, high)
}

plot_theme <- theme_bw() +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.spacing = unit(0, "null"),
    )

gmts_plot <- covidser %>%
    filter(strain == "Wuhan") %>%
    group_by(brand, bleed_day_id) %>%
    summarise(summarise_logmean(ic50), .groups = "drop") %>%
    mutate(
        bleed_day_id = if_else(bleed_day_id == 220, 50, bleed_day_id),
        Brand = brand,
    ) %>%
    ggplot(aes(bleed_day_id, mean, col = Brand, shape = Brand)) +
    plot_theme +
    scale_y_log10("Wuhan GMT (95% CI)") +
    scale_x_continuous(breaks = c(0, 14, 50), labels = c("Pre-vax", "Post-dose 2 day 14", "Post-season")) +
    scale_color_viridis_d(begin = 0.2, end = 0.8) +
    geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 5))

ggsave("covax-paper/wuhan-gmts.pdf", gmts_plot, width = 11, height = 11, units = "cm")

gmts_plot_bmi <- covidser %>%
    filter(strain == "Wuhan") %>%
    group_by(brand, bleed_day_id, bmi_cat) %>%
    summarise(summarise_logmean(ic50), .groups = "drop") %>%
    mutate(
        bleed_day_id = if_else(bleed_day_id == 220, 50, bleed_day_id),
        Brand = brand,
        BMI = bmi_cat,
    ) %>%
    filter(!is.na(low)) %>%
    ggplot(aes(bleed_day_id, mean, col = BMI, shape = BMI)) +
    plot_theme +
    facet_wrap(~Brand) +
    scale_y_log10("Wuhan GMT (95% CI)") +
    scale_x_continuous(breaks = c(0, 14, 50), labels = c("Pre-vax", "Post-dose 2 day 14", "Post-season")) +
    scale_color_viridis_d(begin = 0.2, end = 0.8) +
    geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 10))

ggsave("covax-paper/wuhan-gmts-bmi.pdf", gmts_plot_bmi, width = 15, height = 10, units = "cm")

gmts_plot_comorbidities <- colnames(covidser)[str_starts(colnames(covidser), "condition_")] %>%
    map_dfr(function(colname) {
        colnameq <- rlang::sym(colname)
        covidser %>%
            filter(strain == "Wuhan") %>%
            group_by(brand, bleed_day_id, !!colnameq) %>%
            summarise(summarise_logmean(ic50), .groups = "drop") %>%
            mutate(cond = colname, status = !!colnameq) %>%
            select(brand, bleed_day_id, cond, status, mean, low, high)
    }) %>%
    mutate(
        bleed_day_id = if_else(bleed_day_id == 220, 50, bleed_day_id),
        Brand = brand,
        `Comorbidity status` = factor(status, levels = c("0", "1"), labels = c("No", "Yes")),
        cond = str_replace(cond, "^condition_", ""),
    ) %>%
    filter(!is.na(low)) %>%
    group_by(cond) %>%
    filter(any(`Comorbidity status` == "Yes")) %>%
    ungroup() %>%
    ggplot(aes(bleed_day_id, mean, col = `Comorbidity status`, shape = `Comorbidity status`)) +
    plot_theme +
    facet_grid(cond~Brand) +
    scale_y_log10("Wuhan GMT (95% CI)") +
    scale_x_continuous(breaks = c(0, 14, 50), labels = c("Pre-vax", "Post-dose 2 day 14", "Post-season")) +
    scale_color_viridis_d(begin = 0.2, end = 0.8) +
    geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 10))

ggsave("covax-paper/wuhan-gmts-comorbidities.pdf", gmts_plot_comorbidities, width = 15, height = 30, units = "cm")


