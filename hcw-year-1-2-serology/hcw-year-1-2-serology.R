library(tidyverse)

participants <- read_csv("data/participants.csv", col_types = cols())
serology <- read_csv("data/serology.csv", col_types = cols())
workdept <- read_csv("data/workdept.csv", col_types = cols())
comorbidities <- read_csv("data/comorbidities.csv", col_types = cols())
vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())
yearly_changes <- read_csv("data/yearly-changes.csv", col_types = cols())
bleed_dates <- read_csv("data/bleed-dates.csv", col_types = cols())

summarise_median_range <- function(arr) {
    f <- function(x, q) round(quantile(x, q, na.rm = TRUE))
    paste0(f(arr, 0.5), " (", f(arr, 0.25), ", ", f(arr, 0.75), ")")
}

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

participants_for_summary <- participants %>%
    left_join(summarise(workdept, .by = pid, dept = paste0(dept[status == 1], collapse = ",")), "pid") %>%
    left_join(summarise(comorbidities, .by = pid, medicalcond = paste0(condition, collapse = ",")), "pid") %>%
    mutate(
        # NOTE(sen) Remove dates of birth that don't make sense
        dob = if_else(age_screening < 18 | age_screening > 65, NA_Date_, dob),
        dept = if_else(str_detect(dept, ","), "Multiple", dept),
        dept = if_else(dept == "", NA_character_, dept),
        medicalcond = if_else(str_detect(medicalcond, ","), "Multiple", medicalcond),
        medicalcond = if_else(medicalcond == "", "None", medicalcond),
        medicalcond = replace_na(medicalcond, "None"),
    ) %>%
    left_join(
        serology %>%
            summarise(.by = pid, has2020 = 2020 %in% year, has2021 = 2021 %in% year),
        "pid"
    ) %>%
    (function(data) {
        contrubute2020 <- data %>% filter(has2020) %>% mutate(contribute_year = 2020)
        contrubute2021 <- data %>% filter(has2021) %>% mutate(contribute_year = 2021)
        bind_rows(contrubute2020, contrubute2021)
    }) %>%
    left_join(
        vaccinations %>%
            filter(year == 2020 | year == 2021, status == "Australia" | status == "Overseas") %>%
            select(pid, year, vaccine_brand = brand) %>%
            mutate(vaccinated = TRUE),
        c("pid", "contribute_year" = "year")
    ) %>%
    left_join(prior_vaccinations,c("pid", "contribute_year" = "year")) %>%
    left_join(
        yearly_changes %>% summarise(.by = c(pid, redcap_project_year), children = first(children), emp_status = first(emp_status), occupation = first(occupation)),
        c("pid", "contribute_year" = "redcap_project_year")
    ) %>%
    mutate(
        prior_vax = factor(prior_vax),
        vaccinated = replace_na(vaccinated, FALSE),
        vaccine_brand = case_when(
            vaccinated & is.na(vaccine_brand) ~ "Unknown",
            !vaccinated ~ "Unvaccinated",
            TRUE ~ vaccine_brand
        ),
        vaccine_brand = factor(vaccine_brand, c("GSK", "Sanofi", "Seqirus")),
        gender = factor(gender, c("female", "male", "other")),
        emp_status = factor(emp_status, c("Full time", "Part time", "Casual")),
        age_contribute_year = (ymd(paste0(contribute_year, "-04-01")) - dob) / lubridate::dyears(1),
        years_employed_contribute_year = years_employed + (contribute_year - recruitment_year),
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

participants_for_summary %>%
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
    filter(!var %in% c("None_medicalcond", "missing_medicalcond", "missing_vaccine_brand", "missing_prior_vax")) %>%
    pivot_wider(names_from = "contribute_year", values_from = "summ") %>%
    write_csv("hcw-year-1-2-serology/hcw-year-1-2-serology.csv")

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

add_empty_rows_before <- function(data, row_count, ...) {
    data %>%
        group_by(...) %>%
        group_map(~bind_rows(slice(.y, rep(1, row_count)), .x), .keep = TRUE) %>%
        bind_rows()
}

serology_for_tables <- serology %>%
    filter(virus_vaccine, year == 2020 | year == 2021, vax_inf == "V", day != 7) %>%
    filter(.by = pid, 14 %in% day) %>%
    left_join(prior_vaccinations, c("pid", "year")) %>%
    mutate(subtype = factor(subtype, c("H1", "H3", "BVic", "BYam"), c("A(H1N1)pdm09", "A(H3N2)", "B(Victoria)", "B(Yamagata)")))

# NOTE(sen) Look bleed dates
serology_for_tables %>%
    left_join(
        bleed_dates %>% rename(bleed_date = date),
        c("pid", "year", "day")
    ) %>%
    select(pid, year, day, bleed_date) %>%
    distinct() %>%
    pivot_wider(names_from = "day", values_from = "bleed_date") %>%
    mutate(
        d0_14 = as.integer(`14` - `0`),
        d14_220 = as.integer(`220` - `14`),
    ) %>%
    summarise(
        d0_14_miqr = summarise_median_range(d0_14),
        d14_220_miqr = summarise_median_range(d14_220),
        d220months = paste(quantile(month(`220`), 0.05, na.rm = TRUE), quantile(month(`220`), 0.95, na.rm = TRUE)),
    )

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

fit_results <- serology_for_tables_ratios %>%
    left_join(participants, "pid") %>%
    left_join(participants_for_summary %>% select(pid, contribute_year, age_contribute_year, vaccine_brand), c("pid", "year" = "contribute_year")) %>%
    mutate(
        site = factor(site, c("sydney", "newcastle", "perth", "brisbane", "melbourne", "adelaide")),
        logratio = log(ratio), log14 = log(`14`),
        prior_vax = factor(prior_vax),
        has_comorbidity = as.integer(pid %in% comorbidities$pid),
        notfemale = as.integer(gender != "female"),
        vaccine_brand = factor(vaccine_brand, c("Sanofi", "GSK", "Seqirus")),
    ) %>%
    filter(
        age_screening >= 18, age_screening <= 65,
        weight >= 30, weight <= 250, 
        height >= 100, height <= 250,
    ) %>%
    group_by(subtype, virus_egg_cell, year) %>%
    group_map(function(data, key) {
        outcomes <- c("logratio", "log14")
        covariates <- c("1", "prior_vax", "bmi", "has_comorbidity", "age_contribute_year", "notfemale", "vaccine_brand", "site")
        covtogether <- paste(covariates[2:length(covariates)], collapse = " + ")
        cov_all <- c(covariates, covtogether)
        grid <- expand.grid(outcomes, cov_all)
        formulas <- paste(grid$Var1, "~", grid$Var2)
        map(formulas, function(formula) {
            fit <- lm(as.formula(formula), data)
            fit$key <- key
            fit
        })
    }) %>%
    flatten()

residuals_plots <- fit_results %>%
    map_dfr((function(fit) {
        outcome <- names(fit$model)[[1]]
        covariates <- names(fit$model)
        result <- NULL
        if (length(covariates) > 1) {
            covariates <- covariates[2:length(covariates)]
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
            mutate(outcome = outcome, crude = length(covariates) <= 2, formula = paste0(deparse(formula(fit)), collapse = "")) %>%
            bind_cols(fit$key)
    })) %>%
    group_by(crude) %>%
    group_map(.keep = TRUE, function(data, key) {
        pl <- ggplot(data, aes(cov_value, residuals)) +
            facet_grid(subtype + virus_egg_cell + year ~ cov + outcome, scales = "free_x") +
            theme_bw() +
            theme(panel.spacing = unit(0, "null")) +
            geom_hline(yintercept = 0) +
            geom_point(shape = 18, alpha = 0.3)
        attr(pl, "key") <- key
        pl
    })

walk(residuals_plots, function(pl) {
    crude <- if_else(attr(pl, "key")$crude, "crude", "adjusted")
    name <- paste0("hcw-year-1-2-serology/residuals-plot-", crude, ".pdf")
    ggsave(name, pl, width = 50, height = 50, units = "cm")
})

fit_results %>%
    map_dfr(function(fit) {
        outcome <- names(fit$model)[[1]]
        covariates <- names(fit$model)
        broom::tidy(fit) %>% bind_cols(fit$key) %>%
            mutate(outcome = outcome, crude = length(covariates) <= 2, formula = paste0(deparse(formula(fit)), collapse = ""))
    }) %>%
    mutate(
        estimate_str = paste0(signif(exp(estimate), 2), " (", signif(exp(estimate - 1.96 * std.error), 2), ", ", signif(exp(estimate + 1.96 * std.error), 2), ")"),
        crude_lbl = if_else(crude, "crude", "adjusted"),
        kind = paste0(year, crude_lbl),
    ) %>%
    filter(formula == "log14 ~ 1" | formula == "logratio ~ 1" | (!crude) | term != "(Intercept)") %>%
    select(term, estimate_str, subtype, virus_egg_cell, outcome, kind) %>%
    pivot_wider(names_from = "kind", values_from = "estimate_str") %>%
    group_by(term, subtype, virus_egg_cell, outcome) %>%
    group_map(.keep = TRUE, function(data, key) {
        skips <- c(
            paste0("prior_vax", 1:4), 
            "vaccine_brandGSK",
            "sitenewcastle",
            "siteperth",
            "sitebrisbane",
            "sitemelbourne"
        )
        if (!key$term %in% skips) {
            data <- data %>% bind_rows(key)
        }
        tops <- c("prior_vax1", "vaccine_brandGSK", "sitenewcastle")
        if (key$term %in% tops) {
            data <- bind_rows(key, data)
        }
        if (key$term == "(Intercept)") {
            data <- bind_rows(key, data)
            data <- bind_rows(key, data)
        }
        data
    }) %>%
    bind_rows() %>%
    mutate(
        term = factor(term, c(
            "(Intercept)",
            "prior_vax1",
            "prior_vax2",
            "prior_vax3",
            "prior_vax4",
            "prior_vax5",
            "bmi",
            "has_comorbidity",
            "age_contribute_year",
            "notfemale",
            "vaccine_brandGSK",
            "vaccine_brandSeqirus",
            "sitenewcastle",
            "siteperth",
            "sitebrisbane",
            "sitemelbourne",
            "siteadelaide"
        )),
        across(c(`2020crude`, `2020adjusted`, `2021crude`, `2021adjusted`), ~replace_na(.x, " ")),
    ) %>%
    arrange(outcome, subtype, virus_egg_cell, term) %>%
    write_csv("hcw-year-1-2-serology/fit_results.csv")

save_plot <- function(plot, name, ...) {
    ggsave(glue::glue("hcw-year-1-2-serology/{name}.pdf"), plot, ...)
    ggsave(glue::glue("hcw-year-1-2-serology/{name}.png"), plot, ...)
}

virus_labels <- serology_for_tables %>%
    select(subtype, year, virus, virus_clade, virus_egg_cell) %>% 
    distinct() %>%
    mutate(virus = str_replace(virus, "e$", ""))

gmt_plots <- serology_for_tables %>%
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
        plot <- data %>%
            ggplot(aes(day, mean, col = `Prior vaccinations`, shape = `Prior vaccinations`)) +
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
            )
        attr(plot, "key") <- key
        plot
    })

walk(gmt_plots, function(plot) {
    key <- attr(plot, "key")
    save_plot(plot, glue::glue("gmt_{key$subtype}"), width = 15, height = 15, units = "cm")
})

save_plot(
    ggpubr::ggarrange(plotlist = imap(gmt_plots, ~.x + ggtitle(LETTERS[.y])), common.legend = TRUE),
    "gmt_plot", width = 20, height = 20, units = "cm"
)

summarise_prop3 <- function(arr) {
    success <- sum(arr, na.rm = TRUE)
    total <- sum(!is.na(arr))
    ci <- PropCIs::exactci(success, total, 0.95)
    tibble(mean = success / total, low = ci$conf.int[[1]], high = ci$conf.int[[2]])
}

seropositivity_plots <- serology_for_tables %>%
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
        plot <- data %>%
            ggplot(aes(day, mean, col = `Prior vaccinations`, shape = `Prior vaccinations`)) +
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
            )
        attr(plot, "key") <- key
        plot
    })

walk(seropositivity_plots, function(plot) {
    key <- attr(plot, "key")
    save_plot(plot, glue::glue("seropositivity_{key$subtype}"), width = 15, height = 15, units = "cm")
})

save_plot(
    ggpubr::ggarrange(plotlist = imap(seropositivity_plots, ~.x + ggtitle(LETTERS[.y])), common.legend = TRUE),
    "seropositivity_plot", width = 20, height = 20, units = "cm"
)

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
        glue::glue("{varname1} -> {varname2} [label = 'n={n}']")
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
