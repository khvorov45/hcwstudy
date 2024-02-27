library(tidyverse)

participants <- read_csv("data/participants.csv", col_types = cols())
serology <- read_csv("data/serology.csv", col_types = cols())
workdept <- read_csv("data/workdept.csv", col_types = cols())
comorbidities <- read_csv("data/comorbidities.csv", col_types = cols())
vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())
yearly_changes <- read_csv("data/yearly-changes.csv", col_types = cols())
bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols()) %>% select(pid, year, day, date) %>% distinct()

custom_round <- function(x) {
    if (x >= 10) round(x)
    else if (x >= 1) round(x, 1)
    else round(x, 2)
}

summarise_median_range <- function(arr) {
    f <- function(x, q) custom_round(quantile(x, q, na.rm = TRUE))
    paste0(f(arr, 0.5), " (", f(arr, 0.25), ", ", f(arr, 0.75), ")")
}

summarise_prop <- function(arr) {
    success <- sum(arr, na.rm = TRUE)
    total <- sum(!is.na(arr))
    f <- function(x) paste0(signif(x * 100, 2), "%")
    paste0(success, " (", f(success / total), ")")
}

prior_vaccinations <- vaccinations %>%
    summarise(
        .by = pid,
        `2020` = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        `2021` = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
    ) %>%
    pivot_longer(-pid, names_to = "year", values_to = "prior_vax") %>%
    mutate(year = as.integer(year))

add_total <- function(data) {
    data %>%
        mutate(year = as.character(year)) %>%
        bind_rows(
            data %>%
                inner_join(participants %>% select(pid, recruitment_year), c("pid", "year" = "recruitment_year")) %>%
                mutate(year = "Total")
        )
}

participants_for_summary <- participants %>%
    mutate(
        # NOTE(sen) Remove ages that don't make sense
        age_screening = if_else(age_screening < 18 | age_screening > 65, NA_real_, age_screening),
    ) %>%
    left_join(
        serology %>%
            summarise(.by = pid, has2020 = any(year == 2020 & day == 14), has2021 = any(year == 2021 & day == 14)),
        "pid"
    ) %>%
    (function(data) {
        contrubute2020 <- data %>% filter(has2020) %>% mutate(contribute_year = "2020")
        contrubute2021 <- data %>% filter(has2021) %>% mutate(contribute_year = "2021")
        total <- data %>% filter(has2020 | has2021) %>% mutate(contribute_year = "Total")
        bind_rows(contrubute2020, contrubute2021, total)
    }) %>%
    left_join(
        workdept %>%
            summarise(.by = c(pid, year), dept = paste0(dept, collapse = ",")) %>%
            add_total(),
        c("pid", "contribute_year" = "year")
    ) %>%
    left_join(
        comorbidities %>%
            summarise(.by = c(pid, year), medicalcond = paste0(condition, collapse = ",")) %>%
            add_total(),
        c("pid", "contribute_year" = "year")
    ) %>%
    # TODO(sen) What goes into the "total" column for vaccinations
    left_join(
        vaccinations %>%
            select(pid, year, vaccine_brand = brand) %>%
            mutate(year = as.character(year)),
        c("pid", "contribute_year" = "year")
    ) %>%
    left_join(prior_vaccinations %>% add_total(), c("pid", "contribute_year" = "year")) %>%
    left_join(
        yearly_changes %>%
            rename(year = redcap_project_year) %>%
            summarise(.by = c(pid, year), children = first(children), emp_status = first(emp_status), occupation = first(occupation)) %>%
            add_total(),
        c("pid", "contribute_year" = "year")
    ) %>%
    mutate(
        dept = if_else(str_detect(dept, ","), "Multiple", dept),
        dept = if_else(dept == "", NA_character_, dept),
        medicalcond = if_else(str_detect(medicalcond, ","), "Multiple", medicalcond),
        medicalcond = if_else(medicalcond == "", "None", medicalcond),
        medicalcond = replace_na(medicalcond, "None"),
        prior_vax = factor(prior_vax),
        vaccine_brand = factor(vaccine_brand, c("GSK", "Sanofi", "Seqirus")),
        gender = factor(gender, c("female", "male", "other")),
        emp_status = factor(emp_status, c("Full time", "Part time", "Casual")),
        dob = if_else(!is.na(age_screening), paste0(round(recruitment_year - age_screening), "-07-01"), NA_character_),
        dob = ymd(dob),
        age_contribute_year = if_else(
            contribute_year == "Total",
            (ymd(paste0(recruitment_year, "-04-01")) - dob) / lubridate::dyears(1),
            (ymd(paste0(recode(contribute_year, "Total" = "1000"), "-04-01")) - dob) / lubridate::dyears(1),
        ),
        years_employed_contribute_year = if_else(
            contribute_year == "Total",
            years_employed,
            years_employed + (as.integer(recode(contribute_year, "Total" = "1000")) - recruitment_year)
        ),
        occupation = recode(occupation, "Allied health" = "Other", "Ancillary" = "Other", "Medical" = "Clinical", "Nursing" = "Clinical", "Research" = "Other") %>%
            factor(c("Clinical", "Laboratory", "Administrative", "Other")),
        dept = factor(dept, c(
            "Emergency Department",
            "Critical Care or Intensive Care Unit",
            "General Medicine and/or Medical Specialities",
            "Paediatrics and/or Paediatric Specialities",
            "Surgery and/or Surgical Specialties",
            "Gynaecology and/or Obstetrics",
            "Oncology and/or Haematology",
            "Radiology",
            "Outpatient clinic",
            "Pharmacy",
            "Laboratory",
            "Nutrition",
            "Social Work",
            "Physiotherapy",
            "Occupational therapy",
            "Other",
            "Multiple"
        )),
        medicalcond = factor(medicalcond, c(
            "Cardiac disease",
            "Renal disease",
            "Chronic respiratory condition",
            "Haematological disorder",
            "Chronic neurological condition",
            "Immunocompromising condition",
            "Diabetes or other metabolic disorder",
            "Smoker",
            "Pregnancy",
            "Multiple",
            "None"
        )),
        future_vacc_intent = factor(future_vacc_intent, c("Yes", "No", "Don't know")),
    )

summarise_factor <- function(arr) {
    total = sum(!is.na(arr))
    ogname = deparse(substitute(arr))
    res <- bind_cols(map(levels(arr), function(lvl) {
        count = sum(arr == lvl, na.rm = TRUE)
        res = tibble(.rows = 1)
        res[paste(lvl, ogname, sep = "_")] = paste0(count, " (", signif(count / total * 100, 2), "%)")
        res
    }))
    res[paste0("missing_", ogname)] = paste0(sum(is.na(arr)), " (", as.character(signif(sum(is.na(arr)) / length(arr) * 100, 2)), "%)")
    res
}

baseline_table <- participants_for_summary %>%
    summarise(
        .by = contribute_year,
        age = summarise_median_range(age_contribute_year),
        sex_ = " ",
        summarise_factor(gender),
        years_employed = summarise_median_range(years_employed_contribute_year),
        empstatus_ = " ",
        summarise_factor(emp_status),
        occupation_ = " ",
        summarise_factor(occupation),
        work_area_ = " ",
        summarise_factor(dept),
        medicalcond_ = " ",
        summarise_factor(medicalcond),
        medicalcond_atleast1 = summarise_prop(medicalcond != "None"),
        bmi = summarise_median_range(bmi),
        children = summarise_prop(children),
        vax_ = " ",
        summarise_factor(vaccine_brand),
        prior_vax_ = " ",
        summarise_factor(prior_vax),
        future_vacc_ = " ",
        summarise_factor(future_vacc_intent)
    ) %>%
    mutate(across(everything(), ~replace_na(as.character(.x), "0"))) %>%
    pivot_longer(-contribute_year, names_to = "var", values_to = "summ") %>%
    filter(!var %in% c("None_medicalcond", "missing_medicalcond", "missing_prior_vax")) %>%
    pivot_wider(names_from = "contribute_year", values_from = "summ") %>%
    mutate(Total = if_else(str_ends(var, "_vaccine_brand"), "", Total))

write_csv(baseline_table, "hcw-year-1-2-serology/hcw-year-1-2-serology.csv")

add_empty_rows_before <- function(data, row_count, ...) {
    data %>%
        group_by(...) %>%
        group_map(~bind_rows(slice(.y, rep(1, row_count)), .x), .keep = TRUE) %>%
        bind_rows()
}

# NOTE(sen) Empty - no reason to take day 7 when there is no day 14
# serology %>% filter(7 %in% day, !(14 %in% day))

serology_for_tables <- serology %>%
    filter(virus_vaccine, year == 2020 | year == 2021, vax_inf == "V", day != 7) %>%
    select(-virus_vaccine, -vax_inf) %>%
    filter(.by = c(pid, year), 14 %in% day) %>%
    left_join(prior_vaccinations, c("pid", "year")) %>%
    mutate(subtype = factor(subtype, c("H1", "H3", "BVic", "BYam"), c("A(H1N1)pdm09", "A(H3N2)", "B(Victoria)", "B(Yamagata)")))

# NOTE(sen) Should be empty
serology_for_tables %>% filter(.by = c(pid, year, day, subtype, virus_egg_cell), n() > 1)

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
	string <- glue::glue("{custom_round(mean)} ({custom_round(low)}, {custom_round(high)})")
	tibble(mean, low, high, string)
}

serology_for_tables %>%
    summarise(
        .by = c(subtype, virus_egg_cell, year, day, prior_vax),
        summarise_logmean(titre),
    ) %>%
    select(subtype, virus_egg_cell, year, day, prior_vax, gmt = string) %>%
    arrange(year, subtype, virus_egg_cell, day, prior_vax) %>%
    pivot_wider(names_from = "prior_vax", values_from = "gmt") %>%
    add_empty_rows_before(2, subtype, virus_egg_cell, year) %>%
    add_empty_rows_before(2, year) %>%
    mutate(across(everything(), ~replace_na(as.character(.x), " "))) %>%
    write_csv("hcw-year-1-2-serology/hcw-year-1-2-serology-gmt.csv")

summarise_prop2 <- function(arr) {
    success <- sum(arr, na.rm = TRUE)
    total <- sum(!is.na(arr))
    f <- function(x) paste0(signif(x * 100, 2), "%")
    paste0(success, "/", total, " (", f(success / total), ")")
}

serology_for_tables %>%
    summarise(
        .by = c(subtype, virus_egg_cell, year, day, prior_vax),
        prop = summarise_prop2(titre >= 40),
    ) %>%
    arrange(prior_vax) %>%
    pivot_wider(names_from = "prior_vax", values_from = "prop") %>%
    arrange(year, subtype, virus_egg_cell, day) %>%
    add_empty_rows_before(2, year, subtype, virus_egg_cell) %>%
    add_empty_rows_before(1, year) %>%
    mutate(across(everything(), ~replace_na(as.character(.x), " "))) %>%
    write_csv("hcw-year-1-2-serology/hcw-year-1-2-serology-seropositivity.csv")

serology_for_tables_ratios <- serology_for_tables %>%
    pivot_wider(names_from = "day", values_from = "titre") %>%
    mutate(ratio = `14` / `0`)

serology_for_tables_ratios %>%
    summarise(
        .by = c(subtype, virus_egg_cell, year, prior_vax),
        prop = summarise_prop2(ratio >= 4),
    ) %>%
    arrange(year, prior_vax, subtype, virus_egg_cell) %>%
    pivot_wider(names_from = "prior_vax", values_from = "prop") %>%
    add_empty_rows_before(1, year) %>%
    arrange(year, subtype, virus_egg_cell) %>%
    mutate(across(everything(), ~replace_na(as.character(.x), " "))) %>%
    write_csv("hcw-year-1-2-serology/hcw-year-1-2-serology-seroconversion.csv")

summarise_prop3 <- function(arr) {
    success <- sum(arr, na.rm = TRUE)
    total <- sum(!is.na(arr))
    ci <- PropCIs::exactci(success, total, 0.95)
    tibble(mean = success / total, low = ci$conf.int[[1]], high = ci$conf.int[[2]], prop3_total = total)
}

save_plot <- function(plot, name, ...) {
    ggsave(glue::glue("hcw-year-1-2-serology/{name}.pdf"), plot, ...)
    ggsave(glue::glue("hcw-year-1-2-serology/{name}.png"), plot, ...)
    invisible(plot)
}

serology_for_tables_ratios %>%
    summarise(.by = c(subtype, virus_egg_cell, year, prior_vax, `0`), summarise_prop3(ratio >= 4)) %>%
    filter(!is.na(mean), prop3_total >= 5) %>%
    mutate(`Prior vaccinations` = factor(prior_vax), virus_egg_cell = tools::toTitleCase(virus_egg_cell)) %>%
    group_by(subtype) %>%
    group_map(.keep = TRUE, function(group, key) {
        ggplot(group, aes(`0`, mean, color = `Prior vaccinations`, shape = `Prior vaccinations`, lty = `Prior vaccinations`)) +
        theme_bw() +
        theme(
            legend.position = "top",
            strip.background = element_blank(),
            panel.spacing = unit(0, "null"),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        ) +
        scale_x_log10("Baseline titre", breaks = 5 * 2^(0:20)) +
        scale_y_continuous("Proportion (95% CI) seroconverted (rise >= 4)") +
        scale_color_viridis_d(option = "D", end = 0.8) +
        facet_grid(subtype + virus_egg_cell ~ year) +
        guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1), lty = guide_legend(nrow = 1)) +
        geom_line() +
        geom_point()
    }) %>%
    imap(~.x + ggtitle(LETTERS[.y])) %>%
    ggpubr::ggarrange(plotlist = ., common.legend = TRUE) %>%
    save_plot("seroconv_baseline_prior_plots", width = 20, height = 20, units = "cm")

seroconv_prioronly_plots <- serology_for_tables_ratios %>%
    summarise(.by = c(subtype, virus_egg_cell, year, prior_vax), summarise_prop3(ratio >= 4)) %>%
    filter(!is.na(mean), prop3_total >= 5) %>%
    mutate(`Prior vaccinations` = factor(prior_vax), virus_egg_cell = tools::toTitleCase(virus_egg_cell)) %>%
    ggplot(aes(year, mean, color = `Prior vaccinations`, shape = `Prior vaccinations`, lty = `Prior vaccinations`)) +
    theme_bw() +
    theme(
        legend.position = "top",
        strip.background = element_blank(),
        panel.spacing = unit(0, "null"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
    ) +
    scale_x_continuous(breaks = c(2020, 2021)) +
    scale_y_continuous("Proportion (95% CI) seroconverted (rise >= 4)") +
    scale_color_viridis_d(option = "D", end = 0.8) +
    facet_grid(subtype ~ virus_egg_cell) +
    guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1), lty = guide_legend(nrow = 1)) +
    geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 0.5))

save_plot(seroconv_prioronly_plots, "seroconv_prioronly_plots", width = 15, height = 20, units = "cm")

serology_for_tables_ratios %>%
    filter(!is.na(`0`), !subtype %in% c("B(Victoria)", "B(Yamagata)")) %>%
    mutate(baseline_cat = if_else(`0` < 160, as.character(`0`), ">=160") %>% fct_reorder(`0`)) %>%
    summarise(.by = c(subtype, virus_egg_cell, year, baseline_cat), summarise_prop3(ratio >= 4)) %>%
    filter(!is.na(mean)) %>%
    mutate(
        Baseline = baseline_cat, 
        virus_egg_cell = tools::toTitleCase(virus_egg_cell), 
        good_sample = prop3_total >= 5,
        xpos = year + (as.integer(Baseline) - 3) * 0.1
    ) %>%
    group_by(subtype) %>%
    group_map(function(data, key) {
        (ggplot(data, aes(xpos, mean, color = Baseline, shape = Baseline, lty = Baseline)) +
        theme_bw() +
        theme(
            legend.position = "top",
            strip.background = element_blank(),
            panel.spacing = unit(0, "null"),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
        ) +
        scale_x_continuous(breaks = c(2020, 2021)) +
        scale_y_continuous("Proportion (95% CI) seroconverted (rise >= 4)") +
        scale_color_viridis_d(option = "D", end = 0.8) +
        facet_wrap(~virus_egg_cell) +
        guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1), lty = guide_legend(nrow = 1)) +
        geom_pointrange(aes(ymin = low, ymax = high)) +
        geom_point(aes(xpos, mean), inherit.aes = FALSE, shape = 1, data = filter(data, !good_sample), color = "red", size = 4)) %>%
        save_plot(paste0("seroconv_baselineonly_plots_", key$subtype), width = 20, height = 12, units = "cm")
    })

serology_for_tables_ratios %>%
    mutate(year = as.character(year), virus_egg_cell = tools::toTitleCase(virus_egg_cell)) %>%
    left_join(
        participants_for_summary %>% select(pid, contribute_year, age_contribute_year),
        c("pid", "year" = "contribute_year")
    ) %>%
    (function(data) list("prior_vax" = mutate(data, correlate = prior_vax), "baseline" = mutate(data, correlate = `0`))) %>%
    iwalk(function(data, name) {
        data %>%
        filter(!is.na(age_contribute_year), !is.na(correlate)) %>%
        group_by(subtype) %>%
        mutate(group_index = cur_group_id()) %>%
        group_by(subtype, group_index) %>%
        group_map(.keep = TRUE, function(data, key) {
            ggplot(data, aes(age_contribute_year, correlate)) +
            theme_bw() +
            theme(
                panel.grid.minor = element_blank(),
                panel.spacing = unit(0, "null"),
                strip.background = element_blank(),
            ) +
            list(
                "prior_vax" = scale_y_continuous("Prior vaccinations"),
                "baseline" = list(
                    scale_y_log10("Baseline titre", breaks = 5 * 2^(0:20)),
                    coord_cartesian(ylim = with(serology_for_tables_ratios, c(min(`0`, na.rm = TRUE), max(`0`, na.rm = TRUE))))
                )
            )[[name]] +
            scale_x_continuous("Age (study year)") +
            facet_grid(subtype + virus_egg_cell ~ year) +
            geom_jitter(shape = 18, alpha = 0.1, height = c("prior_vax" = 0.1, "baseline" = 0.02)[name]) +
            geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
            ggtitle(LETTERS[key$group_index])
        }) %>%
        (function(plots) {
            ggpubr::ggarrange(plotlist = plots) %>%
            save_plot(paste0("age_correlation_", name), width = 20, height = 20, units = "cm")
        })
    })

data_for_model_fit <- serology_for_tables_ratios %>%
    left_join(participants, "pid") %>%
    mutate(year = as.character(year)) %>%
    left_join(participants_for_summary %>%
        select(pid, contribute_year, age_contribute_year, vaccine_brand),
        c("pid", "year" = "contribute_year")
    ) %>%
    (function(data) {
        most_common_vaccine_brands <- data %>%
            summarise(.by = c(site, vaccine_brand), n = length(unique(pid))) %>%
            mutate(.by = site, p = n / sum(n)) %>%
            filter(.by = site, p == max(p)) %>%
            filter(.by = site, row_number() == 1) %>%
            filter(p >= 0.95)
        data %>%
            left_join(most_common_vaccine_brands %>% select(site, site_brand_over95 = vaccine_brand), "site") %>%
            mutate(vaccine_brand = if_else(is.na(vaccine_brand), site_brand_over95, vaccine_brand))
    }) %>%
    mutate(
        site = factor(site, c("sydney", "newcastle", "perth", "brisbane", "melbourne", "adelaide")),
        logratio = log(ratio), log14 = log(`14`),
        prior_vax_num = prior_vax,
        prior_vax = factor(prior_vax),
        has_comorbidity = as.integer(pid %in% comorbidities$pid),
        notfemale = as.integer(gender != "female"),
        vaccine_brand = factor(vaccine_brand, c("Sanofi", "GSK", "Seqirus")),
        bmi = bmi - 20,
        age_contribute_year = age_contribute_year - 18,
        baseline_titre = factor(`0`),
        logbaseline = log2(`0`) - log2(5),
        seroconverted = ratio >= 4,
    ) %>%
    filter(
        age_screening >= 18, age_screening <= 65,
        weight >= 30, weight <= 250,
        height >= 100, height <= 250,
    )

fit_results <- data_for_model_fit %>%
    group_by(subtype, virus_egg_cell, year) %>%
    group_map(function(data, key) {
        outcomes <- c("logratio", "log14", "seroconverted")
        covariates <- c("1", "prior_vax", "has_comorbidity", "age_contribute_year", "notfemale", "vaccine_brand")
        covtogether <- paste(covariates[2:length(covariates)], collapse = " + ")
        grid <- expand.grid(outcomes, covariates)
        formulas <- c(
            paste(grid$Var1, "~", grid$Var2), 
            paste0("log14 ~ ", covtogether),
            paste0("logratio ~ ", covtogether, " + baseline_titre"),
            paste0("seroconverted ~ ", covtogether, " + logbaseline")
        )
        map(formulas, function(formula) {
            fit <- NULL
            if (str_starts(formula, "seroconverted")) {
                fit <- glm(as.formula(formula), data, family = "binomial")
            } else {
                fit <- lm(as.formula(formula), data)
            }
            fit$extrainfo <- list(
                key = key,
                outcome = names(fit$model)[[1]],
                covariates = names(fit$model),
                formula = formula
            )
            fit$extrainfo$type <- case_when(
                length(fit$extrainfo$covariates) <= 2 ~ "crude",
                "prior_vax_num" %in% fit$extrainfo$covariates ~ "adjusted_num",
                TRUE ~ "adjusted_cat",
            )
            fit
        })
    }) %>%
    list_flatten()

fit_results %>%
    map_dfr(function(fit) {
        result <- NULL
        if (length(fit$extrainfo$covariates) > 1) {
            covariates <- fit$extrainfo$covariates %>% .[2:length(.)]
            result <- map(covariates, function(cov) {
                    vals <- fit$model[[cov]]
                    if (is.factor(vals)) {
                        vals <- as.integer(vals)
                    }
                    tibble(!!rlang::sym(cov) := vals)
                }) %>%
                bind_cols() %>%
                mutate(residuals = fit$residuals, index = row_number()) %>%
                pivot_longer(-c(residuals, index), names_to = "cov", values_to = "cov_value")
        } else {
            result <- tibble(residuals = fit$residuals, cov = "intercept", cov_value = 0)
        }
        result %>%
            mutate(
                outcome = fit$extrainfo$outcome,
                type = fit$extrainfo$type,
                formula = fit$extrainfo$formula,
            ) %>%
            bind_cols(fit$extrainfo$key)
    }) %>%
    group_by(type) %>%
    group_walk(.keep = TRUE, function(data, key) {
        (ggplot(data, aes(cov_value, residuals)) +
            facet_grid(subtype + virus_egg_cell + year ~ cov + outcome, scales = "free_x") +
            theme_bw() +
            theme(panel.spacing = unit(0, "null")) +
            geom_hline(yintercept = 0) +
            geom_point(shape = 18, alpha = 0.3)) %>%
            ggsave(paste0("hcw-year-1-2-serology/residuals-plot-", key$type, ".pdf"), ., width = 50, height = 50, units = "cm")
    })

summarise_factor2 <- function(arr) {
    total = sum(!is.na(arr))
    ogname = deparse(substitute(arr))
    res <- bind_cols(map(levels(arr), function(lvl) {
        count = sum(arr == lvl, na.rm = TRUE)
        res = tibble(.rows = 1)
        res[paste(ogname, lvl, sep = "")] = paste0(count)
        res
    }))
    res
}

fit_results_formatted <- fit_results %>%
    map_dfr(function(fit) {
        broom::tidy(fit) %>% bind_cols(fit$extrainfo$key) %>%
            mutate(outcome = fit$extrainfo$outcome, type = fit$extrainfo$type, formula = fit$extrainfo$formula)
    }) %>%
    mutate(
        estimate_str = paste0(signif(exp(estimate), 2), " (", signif(exp(estimate - 1.96 * std.error), 2), ", ", signif(exp(estimate + 1.96 * std.error), 2), ")"),
        kind = paste0(year, type),
        term = recode(term,
            "has_comorbidity" = "has_comorbidity1",
            "notfemale" = "notfemale1",
        ),
    ) %>%
    filter(type == "adjusted_cat") %>%
    select(term, estimate_str, subtype, virus_egg_cell, outcome, kind) %>%
    # NOTE(sen) Attach counts
    bind_rows(
        data_for_model_fit %>%
            select(pid, year, subtype, virus_egg_cell, fit_results[[which(map_lgl(fit_results, ~.x$extrainfo$type == "adjusted_cat" && .x$extrainfo$outcome == "log14"))[1]]]$extrainfo$covariates) %>%
            filter(complete.cases(.)) %>%
            select(-log14) %>%
            mutate(outcome = "log14") %>%
            distinct() %>%
            bind_rows(
                data_for_model_fit %>%
                    select(pid, year, subtype, virus_egg_cell, fit_results[[which(map_lgl(fit_results, ~.x$extrainfo$type == "adjusted_cat" && .x$extrainfo$outcome == "logratio"))[1]]]$extrainfo$covariates) %>%
                    filter(complete.cases(.)) %>%
                    select(-logratio) %>%
                    mutate(outcome = "logratio") %>%
                    distinct()
            ) %>%
            bind_rows(
                data_for_model_fit %>%
                    select(pid, year, subtype, virus_egg_cell, fit_results[[which(map_lgl(fit_results, ~.x$extrainfo$type == "adjusted_cat" && .x$extrainfo$outcome == "seroconverted"))[1]]]$extrainfo$covariates) %>%
                    filter(complete.cases(.)) %>%
                    select(-seroconverted) %>%
                    mutate(outcome = "seroconverted") %>%
                    distinct()
            ) %>%
            mutate(
                has_comorbidity = factor(has_comorbidity),
                notfemale = factor(notfemale),
            ) %>%
            summarise(
                .by = c(outcome, year, subtype, virus_egg_cell),
                `(Intercept)` = as.character(n()),
                summarise_factor2(prior_vax),
                summarise_factor2(has_comorbidity),
                summarise_factor2(notfemale),
                summarise_factor2(vaccine_brand),
                summarise_factor2(baseline_titre),
            ) %>%
            mutate(kind = paste0(year, "n")) %>%
            select(-year) %>%
            pivot_longer(c(-outcome, -subtype, -virus_egg_cell, -kind), names_to = "term", values_to = "estimate_str")
    ) %>%
    pivot_wider(names_from = "kind", values_from = "estimate_str") %>%
    select(subtype, virus_egg_cell, outcome, term, `2020n`, `2020adjusted_cat`, `2021n`, `2021adjusted_cat`) %>%
    group_by(term, subtype, virus_egg_cell, outcome) %>%
    group_map(.keep = TRUE, function(data, key) {
        refs_nobind <- c("prior_vax0", "has_comorbidity0", "notfemale0", "vaccine_brandSanofi", "baseline_titre5")
        if (key$term %in% refs_nobind) {
            data <- mutate(data, across(everything(), ~replace_na(.x, "ref")))
        }
        skips <- c(
            paste0("prior_vax", 0:4),
            "has_comorbidity0",
            "notfemale0",
            paste0("vaccine_brand", c("Sanofi", "GSK")),
            "logbaseline",
            paste0("baseline_titre", 5 * 2^(0:11))
        )
        if (key$outcome == "log14") {
            skips <- c(skips, "vaccine_brandSeqirus")
        }
        if (!key$term %in% skips) {
            data <- data %>% bind_rows(key)
        }
        tops <- c("prior_vax0", "vaccine_brandSanofi", "has_comorbidity0", "notfemale0", "baseline_titre5")
        if (key$term %in% tops) {
            data <- bind_rows(key, data)
        }
        data
    }) %>%
    bind_rows() %>%
    mutate(
        term = factor(term, c(
            "(Intercept)",
            "prior_vax_num",
            paste0("prior_vax", 0:5),
            "bmi",
            paste0("has_comorbidity", 0:1),
            "age_contribute_year",
            paste0("notfemale", 0:1),
            paste0("vaccine_brand", c("Sanofi", "GSK", "Seqirus")),
            "logbaseline",
            paste0("baseline_titre", 5 * 2^(0:11))
        )),
        across(starts_with("202"), ~replace_na(.x, " ")),
    ) %>%
    arrange(outcome, subtype, virus_egg_cell, term)

write_csv(fit_results_formatted, "hcw-year-1-2-serology/fit_results.csv")

fit_results_table <- fit_results %>%
    map_dfr(function(fit) {
        if (fit$extrainfo$type == "adjusted_cat") {
            emmeans::emmeans(fit, ~ prior_vax) %>% as_tibble() %>% bind_cols(fit$extrainfo$key) %>%
                mutate(outcome = fit$extrainfo$outcome, formula = paste0(deparse(formula(fit)), collapse = ""))
        } else {
            tibble()
        }
    })

fit_results_table %>%
    mutate(across(c(emmean, lower.CL, upper.CL), exp), virus_egg_cell = tools::toTitleCase(virus_egg_cell), `Prior vaccinations` = prior_vax) %>%
    group_by(outcome) %>%
    group_walk(function(group, key) {
        outcomenames <- c("log14" = "titre", "logratio" = "rise")
        ybreaks <- list("log14" = 5 * 2^(0:20), "logratio" = 2 ^ (0:10))
        (ggplot(group, aes(year, emmean, col = `Prior vaccinations`, shape = `Prior vaccinations`)) +
            theme_bw() +
            theme(
                panel.spacing = unit(0, "null"),
                legend.position = "top",
                panel.grid.minor = element_blank(),
                strip.background = element_blank(),
                axis.title.x = element_blank(),
            ) +
            scale_color_viridis_d(option = "D", end = 0.8) +
            scale_y_log10(glue::glue("Adjusted postvax {outcomenames[key$outcome]} estimate (95% CI)"), breaks = ybreaks[[key$outcome]]) +
            facet_grid(subtype ~ virus_egg_cell) +
            guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
            geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(width = 0.6))) %>%
            save_plot(glue::glue("fit_results_{key$outcome}"), width = 15, height = 20, units = "cm")
    })

virus_labels <- serology_for_tables %>%
    select(subtype, year, virus, virus_clade, virus_egg_cell) %>%
    distinct() %>%
    mutate(virus = str_replace(virus, "e$", ""))

serology_for_tables %>%
    summarise(
        .by = c(subtype, virus_egg_cell, year, day, prior_vax),
        summarise_logmean(titre),
    ) %>%
    mutate(
        `Prior vaccinations` = factor(prior_vax),
        day = if_else(day == 220, 50, day),
    ) %>%
    group_by(subtype) %>%
    group_map(.keep = TRUE, function(data, key) {
        (ggplot(data, aes(day, mean, col = `Prior vaccinations`, shape = `Prior vaccinations`)) +
            theme_bw() +
            theme(
                legend.position = "bottom",
                panel.grid.minor = element_blank(),
                axis.text.x = element_text(angle = 30, hjust = 1),
                axis.title.x = element_blank(),
                strip.background = element_blank(),
            ) +
            guides(
                color = guide_legend(order = 1, nrow = 1),
                shape = guide_legend(order = 1, nrow = 1),
            ) +
            coord_cartesian(ylim = c(5, 1024)) +
            facet_grid(subtype + virus_egg_cell ~ year) +
            scale_x_continuous("Day", breaks = c(0, 14, 50), labels = c("Pre-vaccination", "Post-vaccination", "End-of-year")) +
            scale_y_log10("GMT (95% CI)", breaks = 5 * 2^(0:15)) +
            scale_color_viridis_d(begin = 0, end = 0.8) +
            geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 13), size = 0.3) +
            geom_text(
                aes(label = paste(virus, virus_clade), y = 5, x = -7),
                inherit.aes = FALSE,
                data = virus_labels %>% filter(subtype == key$subtype),
                col = "gray20",
                hjust = 0,
                size = 2,
            )) %>%
            save_plot(glue::glue("gmt_{key$subtype}"), width = 15, height = 15, units = "cm")
    }) %>%
    imap(~.x + ggtitle(LETTERS[.y])) %>%
    ggpubr::ggarrange(plotlist = ., common.legend = TRUE) %>%
    save_plot("gmt_plot", width = 20, height = 20, units = "cm")

serology_for_tables %>%
    summarise(
        .by = c(subtype, virus_egg_cell, year, day, prior_vax),
        summarise_prop3(titre >= 40),
    ) %>%
    mutate(
        `Prior vaccinations` = factor(prior_vax),
        day = if_else(day == 220, 50, day),
    ) %>%
    group_by(subtype) %>%
    group_map(.keep = TRUE, function(data, key) {
        (ggplot(data, aes(day, mean, col = `Prior vaccinations`, shape = `Prior vaccinations`)) +
            theme_bw() +
            theme(
                legend.position = "bottom",
                panel.spacing = unit(0, "null"),
                panel.grid.minor = element_blank(),
                axis.text.x = element_text(angle = 30, hjust = 1),
                axis.title.x = element_blank(),
                strip.background = element_blank(),
            ) +
            guides(
                color = guide_legend(order = 1, nrow = 1),
                shape = guide_legend(order = 1, nrow = 1),
            ) +
            coord_cartesian(ylim = c(0, 1)) +
            facet_grid(subtype + virus_egg_cell ~ year) +
            scale_x_continuous("Day", breaks = c(0, 14, 50), labels = c("Pre-vaccination", "Post-vaccination", "End-of-year")) +
            scale_y_continuous("Seropositive (95% CI)") +
            scale_color_viridis_d(begin = 0, end = 0.8) +
            geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 10)) +
            geom_text(
                aes(label = paste(virus, virus_clade), y = 0.05, x = 0.5),
                inherit.aes = FALSE,
                data = virus_labels %>% filter(subtype == key$subtype),
                col = "gray20",
                hjust = 0,
                size = 2,
                vjust = 1,
            )) %>%
            save_plot(glue::glue("seropositivity_{key$subtype}"), width = 15, height = 15, units = "cm")
    }) %>%
    imap(~.x + ggtitle(LETTERS[.y])) %>%
    ggpubr::ggarrange(plotlist = ., common.legend = TRUE) %>%
    save_plot("seropositivity_plot", width = 20, height = 20, units = "cm")

serology_for_tables %>%
    mutate(
        `Prior vaccinations` = factor(prior_vax),
        virus_egg_cell = tools::toTitleCase(virus_egg_cell),
    ) %>%
    (function(modded_serology) {
        common_els <- list(
            theme_bw(),
            theme(
                panel.spacing = unit(0, "null"),
                strip.background = element_blank(),
                axis.title.x = element_blank(),
                plot.margin = margin(1, 1, 1, 20),
                panel.grid.minor = element_blank(),
                text = element_text(size = 9),
            ),
            guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1), lty = guide_legend(nrow = 1)),
            scale_color_viridis_d(option = "D", end = 0.8)
        )
        gmtlike_plot <- function(data) {
            data %>%
            mutate(day = if_else(day == 220, 50, day)) %>%
            ggplot(aes(day, mean, col = `Prior vaccinations`, shape = `Prior vaccinations`)) +
            common_els +
            theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
            facet_grid(virus_egg_cell ~ year) +
            scale_x_continuous("Day", breaks = c(0, 14, 50), labels = c("Pre-vaccination", "Post-vaccination", "End-of-season")) +
            geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 14))
        }
        gmrlike_plot <- function(data) {
            data %>%
            ggplot(aes(year, mean, col = `Prior vaccinations`, shape = `Prior vaccinations`)) +
            common_els +
            facet_wrap(~ virus_egg_cell, ncol = 1, strip.position = "right") +
            geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 0.75)) +
            scale_x_continuous(breaks = c(2020, 2021))
        }
        all_gmts <- modded_serology %>% summarise(.by = c(subtype, virus, virus_egg_cell, year, day, `Prior vaccinations`), summarise_logmean(titre))
        modded_serology_wide <- modded_serology %>%
            pivot_wider(names_from = "day", values_from = "titre") %>%
            mutate(ratio = `14` / `0`) %>%
            filter(!is.na(ratio))
        all_rises <- modded_serology_wide %>% summarise(.by = c(subtype, virus_egg_cell, year, `Prior vaccinations`), summarise_logmean(ratio))
        max_rise <- max(fit_results_table %>% filter(outcome == "logratio") %>% pull(upper.CL) %>% exp(), all_rises$high)
        max_gmt <- max(fit_results_table %>% filter(outcome == "log14") %>% pull(upper.CL) %>% exp(), all_gmts$high)
        modded_serology %>%
            group_by(subtype) %>%
            group_walk(.keep = TRUE, function(relevant_serology, key) {
                gmt_plot <- all_gmts %>%
                    filter(subtype == key$subtype) %>%
                    gmtlike_plot() +
                    scale_y_log10("GMT (95% CI)", breaks = 5 * 2^(0:20), expand = expansion(0.1)) +
                    coord_cartesian(ylim = c(5, max(max_gmt))) +
                    geom_text(
                        aes(x = 0, y = 5, label = virus),
                        hjust = 0.1, size = 2.5,
                        data = all_gmts %>%
                            filter(subtype == key$subtype) %>%
                            select(year, virus_egg_cell, virus) %>%
                            distinct(),
                        inherit.aes = FALSE,
                    )
                seropositivity_plot <- relevant_serology %>%
                    summarise(.by = c(subtype, virus_egg_cell, year, day, `Prior vaccinations`), summarise_prop3(titre >= 40)) %>%
                    gmtlike_plot() +
                    scale_y_continuous("Proportion (95% CI) seropositive\n(titre >= 40)", breaks = seq(0, 1, 0.1), labels = scales::percent_format()) +
                    coord_cartesian(ylim = c(0, 1))
                relevant_serology_wide <- modded_serology_wide %>% filter(subtype == key$subtype)
                seroconv_plot <- relevant_serology_wide %>%
                    summarise(.by = c(subtype, virus_egg_cell, year, `Prior vaccinations`), summarise_prop3(ratio >= 4)) %>%
                    gmrlike_plot() +
                    scale_y_continuous("Proportion (95% CI) seroconverted\n(rise >= 4)", breaks = seq(0, 1, 0.1), labels = scales::percent_format()) +
                    coord_cartesian(ylim = c(0, 1))
                relevant_fit_results <- fit_results_table %>%
                    filter(subtype == key$subtype) %>%
                    mutate(
                        `Prior vaccinations` = prior_vax,
                        year = as.numeric(year),
                        virus_egg_cell = tools::toTitleCase(virus_egg_cell),
                        mean = exp(emmean),
                        low = exp(lower.CL),
                        high = exp(upper.CL),
                    )
                fit_results_log14 <- filter(relevant_fit_results, outcome == "log14")
                fit_results_logratio <- filter(relevant_fit_results, outcome == "logratio")
                fit_results_log14_plot <- fit_results_log14 %>%
                    ggplot(aes(year, mean, col = `Prior vaccinations`, shape = `Prior vaccinations`)) +
                    common_els +
                    scale_y_log10("Adjusted post-vaccination GMT\n(95% CI)", breaks = 5 * 2^(0:20), expand = expansion(0.1)) +
                    facet_wrap(~ virus_egg_cell, ncol = 1, strip.position = "right") +
                    scale_x_continuous(breaks = c(2020, 2021)) +
                    coord_cartesian(ylim = c(5, max(max_gmt))) +
                    geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 0.75))
                rises_plot <- all_rises %>%
                    filter(subtype == key$subtype) %>%
                    select(mean, low, high, virus_egg_cell, year, `Prior vaccinations`) %>%
                    mutate(type = "Raw") %>%
                    bind_rows(fit_results_logratio %>% select(mean, low, high, virus_egg_cell, year, `Prior vaccinations`) %>% mutate(type = "Adjusted")) %>%
                    mutate(type = factor(type, c("Raw", "Adjusted"))) %>%
                    ggplot(aes(year, mean, col = `Prior vaccinations`, shape = `Prior vaccinations`)) +
                    common_els +
                    facet_grid(virus_egg_cell ~ type) +
                    scale_y_log10("GMR (95% CI)", breaks = 2^(0:10)) +
                    scale_x_continuous(breaks = c(2020, 2021)) +
                    coord_cartesian(ylim = c(1, max_rise)) +
                    geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 0.75))                
                baselineprior_plot <- relevant_serology_wide %>%
                    summarise(.by = c(subtype, virus_egg_cell, year, prior_vax, `0`), summarise_prop3(ratio >= 4)) %>%
                    filter(!is.na(mean)) %>%
                    mutate(`Prior vaccinations` = factor(prior_vax), virus_egg_cell = tools::toTitleCase(virus_egg_cell)) %>%
                    ggplot(aes(`0`, mean, color = `Prior vaccinations`, shape = `Prior vaccinations`, lty = `Prior vaccinations`)) +
                    common_els +
                    theme(
                        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                        axis.title.x = element_text(),
                    ) +
                    scale_x_log10("Baseline titre", breaks = 5 * 2^(0:20)) +
                    scale_y_continuous("Proportion seroconverted\n(rise >= 4)", breaks = seq(0, 1, 0.1), labels = scales::percent_format()) +
                    facet_grid(virus_egg_cell ~ year) +
                    geom_line() +
                    geom_point()                    
                legend <- relevant_serology %>%
                    ggplot(aes(color = `Prior vaccinations`, shape = `Prior vaccinations`, lty = `Prior vaccinations`)) +
                    common_els +
                    theme(
                        axis.text.y = element_text(size = 10),
                        axis.text.x = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title = element_blank(),
                        panel.border = element_blank(),
                        panel.grid = element_blank(),
                        legend.direction = "vertical"
                    ) +
                    geom_pointrange(aes(1, `Prior vaccinations`, xmin = 0.9, xmax = 1.1)) +
                    scale_y_discrete(position = "right") +
                    ggtitle("Prior vaccinations")
                addtitle <- function(title, pos) list(labs(tag = title), theme(plot.tag.position = pos, plot.tag = element_text(vjust = 1, hjust = 0)))
                arranged <- ggpubr::ggarrange(
                    ggpubr::ggarrange(
                        gmt_plot + addtitle("A", c(-0.075, 1)), 
                        ggpubr::ggarrange(NULL, fit_results_log14_plot + addtitle("B", c(-0.075, 1.1)), NULL, ncol = 1, heights = c(0.1, 1, 0.15), legend = "none"),
                        nrow = 1, common.legend = TRUE, legend = "none", widths = c(1, 0.65)
                    ),
                    ggpubr::ggarrange(
                        ggpubr::ggarrange(NULL, seroconv_plot + addtitle("C", c(-0.12, 1.1)), NULL, ncol = 1, heights = c(0.1, 1, 0.05), legend = "none"), 
                        baselineprior_plot + addtitle("D", c(-0.075, 1)), 
                        nrow = 1, common.legend = TRUE, legend = "none", widths = c(0.65, 1)
                    ),
                    ggpubr::ggarrange(
                        seropositivity_plot + addtitle("E", c(-0.075, 1)),
                        NULL, ggpubr::ggarrange(NULL, legend, NULL, ncol = 1, legend = "none", heights = c(1, 2, 1)), NULL, 
                        nrow = 1, common.legend = TRUE, legend = "none", widths = c(4, 1, 1, 1)
                    ),
                    heights = c(1, 1, 1),
                    ncol = 1,
                    common.legend = TRUE
                )
                save_plot(arranged, paste0("combined-", key$subtype), width = 14, height = 21, units = "cm")
            })
    })

contributing_participants <- serology %>%
    select(pid, year, day) %>%
    bind_rows(bleed_dates %>% select(pid, year, day)) %>%
    distinct() %>%
    filter(day %in% c(0, 14, 220), year %in% c(2020, 2021)) %>%
    left_join(participants %>% select(pid, recruitment_year), "pid")

contributing_participants_wider <- contributing_participants %>%
    mutate(contributed = TRUE, yearday = paste0(year, "_", day)) %>%
    select(-year, -day) %>%
    pivot_wider(names_from = "yearday", values_from = "contributed")

addifnotempty <- function(varname1, varname2, subset) {
    n <- nrow(subset)
    if (n > 0) {
        glue::glue("{varname1} -> {varname2} [label = 'n={n}', penwidth = {n / 1000 * 3 + 0.1}]")
    } else {
        ""
    }
}

strobe <- DiagrammeR::grViz(glue::glue("digraph {{
    graph[layout = dot, rankdir = TD]

    cons2020[label = 'Consented HCWs in 2020\nn={participants %>% filter(year(date_screening) == 2020) %>% nrow()}']
    prevax2020[label = 'Pre-vaccination 2020\nn={contributing_participants %>% filter(year == 2020, day == 0) %>% nrow()}']
    postvax2020[label = 'Post-vaccination 2020\nn={contributing_participants %>% filter(year == 2020, day == 14) %>% nrow()}']
    postseason2020[label = 'Post-season 2020\nn={contributing_participants %>% filter(year == 2020, day == 220) %>% nrow()}']

    cons2021[label = 'Consented HCWs in 2021\nn={participants %>% filter(year(date_screening) == 2021) %>% nrow()}']
    prevax2021[label = 'Pre-vaccination 2021\nn={contributing_participants %>% filter(year == 2021, day == 0) %>% nrow()}']
    postvax2021[label = 'Post-vaccination 2021\nn={contributing_participants %>% filter(year == 2021, day == 14) %>% nrow()}']
    postseason2021[label = 'Post-season 2021\nn={contributing_participants %>% filter(year == 2021, day == 220) %>% nrow()}']

    lost[label = 'Lost to follow-up\nn={participants %>% filter(recruitment_year %in% c(2020, 2021)) %>% select(pid) %>% left_join(contributing_participants_wider, 'pid') %>% filter(is.na(`2021_220`)) %>% nrow()}']

    {addifnotempty('cons2020', 'prevax2020', contributing_participants_wider %>% filter(recruitment_year == 2020, !is.na(`2020_0`)))}
    {addifnotempty('cons2020', 'postvax2020', contributing_participants_wider %>% filter(recruitment_year == 2020, is.na(`2020_0`), !is.na(`2020_14`)))}
    {addifnotempty('cons2020', 'postseason2020', contributing_participants_wider %>% filter(recruitment_year == 2020, is.na(`2020_0`), is.na(`2020_14`), !is.na(`2020_220`)))}
    {addifnotempty('cons2020', 'prevax2021', contributing_participants_wider %>% filter(recruitment_year == 2020, is.na(`2020_0`), is.na(`2020_14`), is.na(`2020_220`), !is.na(`2021_0`)))}
    {addifnotempty('cons2020', 'postvax2021', contributing_participants_wider %>% filter(recruitment_year == 2020, is.na(`2020_0`), is.na(`2020_14`), is.na(`2020_220`), is.na(`2021_0`), !is.na(`2021_14`)))}
    {addifnotempty('cons2020', 'postseason2021', contributing_participants_wider %>% filter(recruitment_year == 2020, is.na(`2020_0`), is.na(`2020_14`), is.na(`2020_220`), is.na(`2021_0`), is.na(`2021_14`), !is.na(`2021_220`)))}
    {addifnotempty('cons2020', 'lost', participants %>% filter(recruitment_year == 2020) %>% left_join(contributing_participants %>% filter(recruitment_year == 2020) %>% select(pid) %>% distinct() %>% mutate(cont = TRUE), 'pid') %>% filter(is.na(cont)))}

    {addifnotempty('cons2021', 'prevax2021', contributing_participants_wider %>% filter(recruitment_year == 2021, !is.na(`2021_0`)))}
    {addifnotempty('cons2021', 'postvax2021', contributing_participants_wider %>% filter(recruitment_year == 2021, is.na(`2021_0`), !is.na(`2021_14`)))}
    {addifnotempty('cons2021', 'postseason2021', contributing_participants_wider %>% filter(recruitment_year == 2021, is.na(`2021_0`), is.na(`2021_14`), !is.na(`2021_220`)))}
    {addifnotempty('cons2021', 'lost', participants %>% filter(recruitment_year == 2021) %>% left_join(contributing_participants %>% filter(recruitment_year == 2021) %>% select(pid) %>% distinct() %>% mutate(cont = TRUE), 'pid') %>% filter(is.na(cont)))}

    {addifnotempty('prevax2020', 'postvax2020', contributing_participants_wider %>% filter(!is.na(`2020_0`), !is.na(`2020_14`)))}
    {addifnotempty('prevax2020', 'postseason2020', contributing_participants_wider %>% filter(!is.na(`2020_0`), is.na(`2020_14`), !is.na(`2020_220`)))}
    {addifnotempty('prevax2020', 'prevax2021', contributing_participants_wider %>% filter(!is.na(`2020_0`), is.na(`2020_14`), is.na(`2020_220`), !is.na(`2021_0`)))}
    {addifnotempty('prevax2020', 'postvax2021', contributing_participants_wider %>% filter(!is.na(`2020_0`), is.na(`2020_14`), is.na(`2020_220`), is.na(`2021_0`), !is.na(`2021_14`)))}
    {addifnotempty('prevax2020', 'postseason2021', contributing_participants_wider %>% filter(!is.na(`2020_0`), is.na(`2020_14`), is.na(`2020_220`), is.na(`2021_0`), is.na(`2021_14`), !is.na(`2021_220`)))}
    {addifnotempty('prevax2020', 'lost', contributing_participants_wider %>% filter(!is.na(`2020_0`), is.na(`2020_14`), is.na(`2020_220`), is.na(`2021_0`), is.na(`2021_14`), is.na(`2021_220`)))}

    {addifnotempty('postvax2020', 'postseason2020', contributing_participants_wider %>% filter(!is.na(`2020_14`), !is.na(`2020_220`)))}
    {addifnotempty('postvax2020', 'prevax2021', contributing_participants_wider %>% filter(!is.na(`2020_14`), is.na(`2020_220`), !is.na(`2021_0`)))}
    {addifnotempty('postvax2020', 'postvax2021', contributing_participants_wider %>% filter(!is.na(`2020_14`), is.na(`2020_220`), is.na(`2021_0`), !is.na(`2021_14`)))}
    {addifnotempty('postvax2020', 'postseason2021', contributing_participants_wider %>% filter(!is.na(`2020_14`), is.na(`2020_220`), is.na(`2021_0`), is.na(`2021_14`), !is.na(`2021_220`)))}
    {addifnotempty('postvax2020', 'lost', contributing_participants_wider %>% filter(!is.na(`2020_14`), is.na(`2020_220`), is.na(`2021_0`), is.na(`2021_14`), is.na(`2021_220`)))}

    {addifnotempty('postseason2020', 'prevax2021', contributing_participants_wider %>% filter(!is.na(`2020_220`), !is.na(`2021_0`)))}
    {addifnotempty('postseason2020', 'postvax2021', contributing_participants_wider %>% filter(!is.na(`2020_220`), is.na(`2021_0`), !is.na(`2021_14`)))}
    {addifnotempty('postseason2020', 'postseason2021', contributing_participants_wider %>% filter(!is.na(`2020_220`), is.na(`2021_0`), is.na(`2021_14`), !is.na(`2021_220`)))}
    {addifnotempty('postseason2020', 'lost', contributing_participants_wider %>% filter(!is.na(`2020_220`), is.na(`2021_0`), is.na(`2021_14`), is.na(`2021_220`)))}

    {addifnotempty('prevax2021', 'postvax2021', contributing_participants_wider %>% filter(!is.na(`2021_0`), !is.na(`2021_14`)))}
    {addifnotempty('prevax2021', 'postseason2021', contributing_participants_wider %>% filter(!is.na(`2021_0`), is.na(`2021_14`), !is.na(`2021_220`)))}
    {addifnotempty('prevax2021', 'lost', contributing_participants_wider %>% filter(!is.na(`2021_0`), is.na(`2021_14`), is.na(`2021_220`)))}

    {addifnotempty('postvax2021', 'postseason2021', contributing_participants_wider %>% filter(!is.na(`2021_14`), !is.na(`2021_220`)))}
    {addifnotempty('postvax2021', 'lost', contributing_participants_wider %>% filter(!is.na(`2021_14`), is.na(`2021_220`)))}
}}"))

strobe_raw <- strobe %>% DiagrammeRsvg::export_svg() %>% charToRaw()

rsvg::rsvg_pdf(strobe_raw, "hcw-year-1-2-serology/strobe.pdf")
rsvg::rsvg_png(strobe_raw, "hcw-year-1-2-serology/strobe.png")

all_dates <- bleed_dates %>%
    filter(day %in% c(0, 14, 220)) %>%
    mutate(day = as.character(day)) %>%
    bind_rows(vaccinations %>% filter(!is.na(vaccination_date)) %>% select(pid, year, date = vaccination_date) %>% mutate(day = "vax")) %>%
    filter(year %in% c(2020, 2021)) %>%
    mutate(day = factor(day, levels = c("0", "vax", "14", "220"), labels = c("Pre-vaccination", "Vaccination", "Post-vaccination", "End-of-year"))) %>%
    arrange(pid, year, day) %>%
    # NOTE(sen) Filter dates that don't make sense (likely data entry errors)
    filter(.by = c(pid, year), date - lag(date, default = ymd("2000-01-01")) > -3)

bleed_intervals <- all_dates %>%
    pivot_wider(names_from = "day", values_from = "date") %>%
    mutate(
        i0_vax = `Vaccination` - `Pre-vaccination`,
        ivax_14 = `Post-vaccination` - `Vaccination`,
        ivax_220 = `End-of-year` - `Vaccination`,
    )

bleed_intervals_plotted <- bleed_intervals %>%
    select(pid, year, starts_with("i")) %>%
    pivot_longer(starts_with("i"), names_to = "timepoint", values_to = "interval") %>%
    filter(!is.na(interval)) %>%
    mutate(
        interval = as.integer(interval),
        timepoint = factor(timepoint, c("i0_vax", "ivax_7", "ivax_14", "ivax_220", "i0_flupos", "ivax_flupos"))
    )

all_dates %>%
    left_join(bleed_intervals, c("pid", "year")) %>%
    group_by(year) %>%
    group_map(.keep = TRUE, function(data, key) {
        this_year <- key$year
        date_min <- lubridate::ymd(glue::glue("{this_year}-03-01"))
        date_max <- lubridate::ymd(glue::glue("{this_year + 1}-01-01"))
        pl <- data %>%
            mutate(
                pid = fct_reorder(fct_drop(pid), replace_na(as.integer(`Pre-vaccination`), 0), .desc = TRUE),
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
                plot.margin = margin(),
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
            geom_point(data = . %>% filter(day == "vax")) +
            geom_text(
                aes(x = date_min, y = 10, label = glue::glue("Participants bled: {pids}")),
                hjust = 0,
                inherit.aes = FALSE, data = summarise(contributing_participants, .by = year, pids = length(unique(pid))) %>% filter(year == this_year)
            )
        if (this_year != max(all_dates$year)) {
            pl <- pl + theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.ticks.length.x = unit(0, "null"),
            )
        }
        pl
    }) %>%
    ggpubr::ggarrange(plotlist = ., common.legend = TRUE, ncol = 1, heights = c(0.95, 1)) %>%
    (function(bleed_dates_plot) {
        (ggplot(bleed_intervals_plotted, aes(timepoint, interval)) +
            theme_bw() +
            theme(
                panel.grid.minor = element_blank(),
                strip.background = element_blank(),
                panel.spacing = unit(0, "null"),
                axis.text.x = element_text(angle = 30, hjust = 1),
                axis.title.x = element_blank(),
                plot.margin = margin(),
            ) +
            facet_wrap(~year, ncol = 1, strip.position = "right") +
            scale_y_continuous("Days", breaks = c(0, 14, 30, 60, 90, 120, 150, 180, 220)) +
            scale_x_discrete("Interval", labels = c("Prevaccination to vaccination", "Vaccination to post-vaccination", "Vaccination to end-of-year")) +
            geom_jitter(shape = 18, width = 0.2, alpha = 0.3, color = "gray50") +
            geom_boxplot(color = "blue", fill = NA, outlier.alpha = 0)) %>%
            ggpubr::ggarrange(ggplot() + theme_minimal(), ., ncol = 1, heights = c(0.06, 1)) %>%
            ggpubr::ggarrange(bleed_dates_plot %>% ggpubr::ggarrange(ggplot() + theme_minimal(), ncol = 1, heights = c(1, 0.137)), ., widths = c(1, 0.5)) %>%
            save_plot("bleed_intervals_and_dates", width = 25, height = 18, units = "cm")
    })

library(officer)
library(flextable)

set_flextable_defaults(big.mark = "")

read_docx() %>%
    body_add_par("Baseline characteristics") %>%
    body_add_table(
        baseline_table %>%
            mutate(
                var = recode(var,
                        "empstatus_" = "Employment status",
                        "age" = "Age, median (range), yr",
                        "bmi" = "BMI, median (range)",
                        "children" = "Has children under 12 years of age living in household",
                        "years_employed" = "Number of years employed, median (range), yr",
                        "work_area_" = "Work area",
                        "vax_" = "Vaccine brand",
                        "prior_vax_" = "Prior vaccinations",
                        "future_vacc_" = "Intends to get vaccinated next year",
                        "medicalcond_" = "Health conditions",
                        "medicalcond_atleast1" = "​  At least 1 condition",
                    ) %>%
                    (function(var) {
                        case_when(
                            str_ends(var, "_") ~ tools::toTitleCase(var) %>% str_replace_all("_", ""),
                            str_ends(var, "_gender") ~ str_replace(var, "_gender$", "") %>% tools::toTitleCase() %>% paste0("​  ", .),
                            str_detect(var, "_") ~ paste0("​  ", var) %>% str_replace("_.*$", ""),
                            TRUE ~ var,
                        )
                    }) %>%
                    str_replace("missing", "Missing")
            ) %>%
            bind_rows(tibble(var = " ", `2020` = "n (%)", `2021` = "n (%)", Total = "n (%)"), .) %>%
            rename(` ` = var, `Total*` = Total)
    ) %>%
    body_add_par("") %>%
    body_add_par("") %>%
    body_add_par("Bleed intervals") %>%
    reduce(tools::list_files_with_exts("hcw-year-1-2-serology", "png") %>% .[str_detect(., "combined-")], .init = ., function(doc, img) {
        doc %>%
            body_add_par("") %>%
            body_add_par(paste0("Combined plots - ", str_replace(basename(img), "^combined-(.*)\\.[[:alpha:]]{3}$", "\\1"))) %>%
            body_add_img(img, width = 6, height = 6)
    }) %>%
    print(target = "hcw-year-1-2-serology/everything.docx")

read_docx() %>%
    body_add_table(
        bleed_intervals_plotted %>%
            left_join(prior_vaccinations, by = c("pid", "year")) %>%
            mutate(across(c(year, prior_vax), as.character)) %>%
            (function(data) {
                bind_rows(
                    summarise(data, .by = c(timepoint, year, prior_vax), medrange = summarise_median_range(interval)),
                    summarise(data, .by = c(timepoint, year), medrange = summarise_median_range(interval), prior_vax = "total"),
                    summarise(data, .by = c(timepoint, prior_vax), medrange = summarise_median_range(interval), year = "total"),
                    summarise(data, .by = c(timepoint), medrange = summarise_median_range(interval), prior_vax = "total", year = "total"),
                )
            }) %>%
            pivot_wider(names_from = "timepoint", values_from = "medrange") %>%
            arrange(year, prior_vax)
    ) %>%
    print(target = "hcw-year-1-2-serology/bleed-intervals.docx")

read_docx() %>%
    (function(doc) {
        serology_for_tables %>%
            group_by(subtype) %>%
            group_map(function(data, key) {
                summarise_prop_custom <- function(arr) {
                    success <- sum(arr, na.rm = TRUE)
                    total <- sum(!is.na(arr))
                    f <- function(x) paste0(custom_round(x * 100), "%")
                    tibble(mean = success / total, string = paste0(success, "/", total, " (", f(success / total), ")"))
                }
                data_ratios <- data %>%
                    pivot_wider(names_from = "day", values_from = "titre") %>%
                    mutate(ratio = `14` / `0`) %>%
                    filter(!is.na(ratio))
                gmt <- data %>%
                    summarise(.by = c(year, day, virus, virus_egg_cell, prior_vax), summarise_logmean(titre)) %>%
                    mutate(type = "GMT")
                seropos <- data %>%
                    summarise(.by = c(year, day, virus, virus_egg_cell, prior_vax), summarise_prop_custom(titre >= 40)) %>%
                    mutate(type = "Seropositivity")
                gmr <- data_ratios %>%
                    summarise(.by = c(year, virus, virus_egg_cell, prior_vax), summarise_logmean(ratio)) %>%
                    mutate(type = "GMR", day = 14)
                seroconv <- data_ratios %>%
                    summarise(.by = c(year, virus, virus_egg_cell, prior_vax), summarise_prop_custom(ratio >= 4)) %>%
                    mutate(type = "Seroconversion", day = 14)
                tbl <- gmt %>%
                    bind_rows(gmr) %>%
                    bind_rows(seroconv) %>%
                    bind_rows(seropos) %>%
                    select(type, year, virus, virus_egg_cell, day, prior_vax, mean, string) %>%
                    mutate(type = factor(type, c("GMT", "GMR", "Seroconversion", "Seropositivity"))) %>%
                    arrange(prior_vax, type, year, virus, virus_egg_cell, day)
                tibble(
                    tb = list(tb = tbl %>%
                        select(-mean) %>%
                        pivot_wider(names_from = prior_vax, values_from = "string") %>%
                        rename(` ` = type, Year = year, Virus = virus, Substrate = virus_egg_cell, Bleed = day) %>%
                        flextable() %>%
                        theme_booktabs() %>%
                        bg(bg = "white", part = "all") %>%
                        fontsize(size = 9, part = "all") %>%
                        padding(padding = 0, part = "all") %>%
                        # NOTE(sen) Color the table (each row gets its cells colored max->min)
                        (function(doc) {
                            tbl %>%
                                group_by(type, year, virus_egg_cell, day) %>%
                                mutate(rown = cur_group_id()) %>%
                                arrange(mean) %>%
                                group_by(rown, .add = TRUE) %>%
                                group_walk(.keep = TRUE, function(data, key) {
                                    colors <- viridis::magma(nrow(data), direction = -1, end = 0.8)
                                    iwalk(data$prior_vax, function(prior_vax_val, index) {
                                        doc <<- color(doc, i = key$rown, j = 6 + prior_vax_val, color = colors[index])
                                    })
                                })
                            doc
                        }) %>%
                        merge_v(j = 1:4))
                ) %>% bind_cols(key)
            }) %>%
            bind_rows() %>%
            arrange(subtype) %>%
            pwalk(function(tb, subtype) {
                doc <<- doc %>%
                    body_add_par("") %>%
                    body_add_par(subtype) %>%
                    body_add_flextable(tb)
            })
        doc %>%
            body_end_section_landscape()
    }) %>%
    print(target = "hcw-year-1-2-serology/gmt-gmr-seropos-seroconv.docx")

read_docx() %>%
    (function(doc) {
        fit_results_formatted %>%
            group_by(outcome, subtype, virus_egg_cell) %>%
            group_walk(function(group, key) {
                group_fmt <- group %>%
                    (function(group) {
                        if (key$outcome != "logratio") {
                            filter(group, !str_starts(term, "baseline_titre"))
                        } else {
                            group
                        }
                    }) %>%
                    mutate(
                        term = case_when(
                            `2020adjusted_cat` == " " & `2020n` != "0" ~ " ",
                            TRUE ~ term,
                        ) %>%
                            if_else(row_number() == first(which(. == "prior_vax0")) - 1, "Prior vaccinations", .) %>%
                            if_else(row_number() == first(which(. == "vaccine_brandSanofi")) - 1, "Brand", .) %>%
                            if_else(row_number() == first(which(. == "has_comorbidity0")) - 1, "Health conditions", .) %>%
                            if_else(row_number() == first(which(. == "notfemale0")) - 1, "Sex", .) %>%
                            if_else(key$outcome == "logratio" & row_number() == first(which(. == "baseline_titre5")) - 1, "Baseline titre", .) %>%
                            recode(
                                "has_comorbidity0" = "Absence of any health condition",
                                "has_comorbidity1" = "Presence of any health condition",
                                "age_contribute_year" = "Age (ref 18)",
                                "notfemale0" = "Female",
                                "notfemale1" = "Not female",
                                "logbaseline" = "Log2 baseline (ref 5)",
                            ) %>%
                            str_replace("^(prior_vax)|(vaccine_brand)|(baseline_titre)", ""),
                    ) %>%
                    bind_rows(tibble(term = " ", `2020n` = "n", `2020adjusted_cat` = "Estimate", `2021n` = "n", `2021adjusted_cat` = "Estimate"), .) %>%
                    rename(` ` = term, `2020` = `2020n`, `  ` = `2020adjusted_cat`, `2021` = `2021n`, `   ` = `2021adjusted_cat`) %>%
                    flextable() %>%
                    theme_booktabs() %>%
                    bg(bg = "white", part = "all") %>%
                    fontsize(size = 9, part = "all") %>%
                    padding(padding = 0, part = "all") %>%
                    merge_at(i = 1, j = 2:3, part = "header") %>%
                    merge_at(i = 1, j = 4:5, part = "header") %>%
                    align(j = 2:5, align = "center", part = "all") %>%
                    (function(doc) {
                        bolds <- c(2, 2, 8, 4, 2, 4)
                        if (key$outcome != "log14") {
                            bolds <- c(bolds, 5)
                        }
                        bold(doc, i = cumsum(bolds), j = 1)
                    }) %>%
                    autofit()
                doc <<- doc %>%
                    body_add_par(paste("Fit results", key$outcome, key$subtype, key$virus_egg_cell)) %>%
                    body_add_flextable(group_fmt)
            })
        print(doc %>% body_end_section_landscape(), target = "hcw-year-1-2-serology/fit_results.docx")
    })

data_for_model_fit %>%
    group_by(virus, subtype, year, virus_egg_cell) %>%
    group_map(function(data, key) {
        jtest <- PMCMRplus::jonckheereTest(data$log14, data$prior_vax)
        fit <- lm(log14 ~ prior_vax_num, data)
        fit %>%
            broom::tidy() %>%
            mutate(r2 = summary(fit)$r.squared, pval_jtest = jtest$p.value) %>%
            bind_cols(key)
    }) %>%
    bind_rows() %>%
    mutate(estimate_str = glue::glue("{signif(exp(estimate), 2)} ({signif(exp(estimate - 1.96 * std.error), 2)}, {signif(exp(estimate + 1.96 * std.error), 2)})")) %>%
    select(-estimate, -std.error, -statistic) %>%
    pivot_wider(names_from = "term", values_from = c("estimate_str", "p.value")) %>%
    mutate(
        across(c(p.value_prior_vax_num, pval_jtest), function(pv) if_else(pv < 0.001, "<0.001", as.character(signif(pv, 2)))),
    ) %>%
    select(
        Year = year, Subtype = subtype, Substrate = virus_egg_cell,
        Intercept = `estimate_str_(Intercept)`,
        `Estimate for linear prior vaccination` = estimate_str_prior_vax_num,
        `p-value` = p.value_prior_vax_num,
        pval_jtest,
    ) %>%
    arrange(Year, Subtype, Substrate) %>%
    flextable() %>%
    theme_booktabs() %>%
    hline(i = 8) %>%
    fontsize(size = 9, part = "all") %>%
    merge_v(1:3) %>%
    autofit() %>%
    align(align = "center", part = "all") %>%
    (function(table) {
        read_docx() %>%
            body_add_flextable(table) %>%
            print(target = "hcw-year-1-2-serology/prior_vax_linear_fit.docx")
    })
