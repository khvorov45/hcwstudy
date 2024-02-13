library(tidyverse)

serology <- read_csv("data/serology.csv", col_types = cols())
vaccinations <- read_csv("data/vaccinations.csv", col_types = cols())

priors <- vaccinations %>%
    summarise(
        .by = pid,
        prior2020 = sum((status == "Australia" | status == "Overseas") & (year < 2020) & (year >= 2020 - 5)),
        prior2021 = sum((status == "Australia" | status == "Overseas") & (year < 2021) & (year >= 2021 - 5)),
        prior2022 = sum((status == "Australia" | status == "Overseas") & (year < 2022) & (year >= 2022 - 5)),
        prior2023 = sum((status == "Australia" | status == "Overseas") & (year < 2023) & (year >= 2023 - 5)),
    )

serology_relevant <- serology %>%
    filter(day %in% c(0, 14), vax_inf == "V", subtype == "H1", virus_vaccine) %>%
    select(-vax_inf, -subtype, -virus_clade, -virus_vaccine) %>%
    left_join(
        priors %>% 
            pivot_longer(-pid, names_to = "year", values_to = "priors") %>%
            mutate(year = str_replace(year, "prior", "") %>% as.integer()),
        c("pid", "year")
    )

serology_relevant_wide <- serology_relevant %>%
    mutate(day = paste0("d", day)) %>%
    pivot_wider(names_from = "day", values_from = "titre") %>%
    mutate(ratio = d14 / d0)

titre_and_ratio_common <- list(
    coord_cartesian(xlim = c(0.75, 2.25)),
    theme_bw(),
    theme(
        panel.spacing = unit(0, "null"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
    ),
    facet_grid(year~priors, labeller = function(breaks) {
        if ("priors" %in% names(breaks)) {
            breaks$priors <- paste0(breaks$priors, " priors (in serology year)")
        }
        breaks
    }),
    scale_x_continuous(breaks = c(1, 2), labels = c("Cell", "Egg"))
)

pid_jitters <- serology_relevant %>%
    select(pid) %>%
    distinct() %>%
    mutate(
        pid_jitterx = runif(n(), -0.05, 0.05),
        pid_jittery = runif(n(), -0.1, 0.1),
    )

titre_plot <- serology_relevant %>%
    left_join(pid_jitters, "pid") %>%
    mutate(
        xcoord = as.integer(factor(virus_egg_cell)) + (as.integer(factor(day)) - 1.5) * 0.2 + pid_jitterx,
        ycoord = exp(log(titre) + pid_jittery),
    ) %>%
    ggplot(aes(xcoord, ycoord)) +
    titre_and_ratio_common +
    scale_y_log10("Titre (D0 and D14)", breaks = 5 * 2^(0:15)) +
    geom_point(shape = 18, alpha = 0.1, color = "gray50") +
    geom_line(aes(group = paste0(pid, virus_egg_cell)), alpha = 0.01, color = "gray50") +
    geom_boxplot(aes(group = paste0(virus_egg_cell, day)), color = "red", fill = NA, outlier.alpha = 0, width = 0.5)

ratio_plot <- serology_relevant_wide %>%
    filter(!is.na(ratio)) %>%
    mutate(xcoord = as.integer(factor(virus_egg_cell))) %>%
    ggplot(aes(xcoord, ratio)) +
    titre_and_ratio_common +
    scale_y_log10("D14/D0 ratio", breaks = 2^(0:15)) +
    geom_jitter(width = 0.1, alpha = 0.1, shape = 18, color = "gray50") +
    geom_boxplot(aes(group = virus_egg_cell), color = "red", fill = NA, outlier.alpha = 0, width = 0.4)

titre_and_ratio_plot <- ggpubr::ggarrange(titre_plot, ratio_plot, ncol = 1, align = "v")

ggsave("h1-eggcell/titre_and_ratio.pdf", titre_and_ratio_plot, width = 30, height = 25, units = "cm")

serology_relevant_wide_eggcell <- serology_relevant %>%
    select(-virus) %>%
    pivot_wider(names_from = "virus_egg_cell", values_from = "titre") %>%
    mutate(ratio = cell / egg)

serology_relevant_wide_eggcell_plot <- serology_relevant_wide_eggcell %>%
    filter(!is.na(ratio)) %>%
    left_join(pid_jitters, "pid") %>%
    mutate(
        xcoord = as.integer(factor(year)) + 0.2 * (as.integer(factor(day)) - 1.5) + pid_jitterx,
        ycoord = exp(log(ratio) + pid_jittery),
    ) %>%
    ggplot(aes(xcoord, ycoord)) +
    theme_bw() +
    theme(
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        panel.spacing = unit(0, "null"),
    ) +
    scale_y_log10("Cell/egg titre ratio (D0 and D14)", breaks = 2^(-10:10)) +
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("2020", "2021", "2022")) +
    coord_cartesian(ylim = c(2^(-8), 64)) +
    geom_hline(yintercept = 1, lty = "11", col = "gray50") +
    geom_point(alpha = 0.1, shape = 18, color = "gray50") +
    geom_line(aes(group = paste0(pid, year)), alpha = 0.1, color = "gray50") +
    geom_boxplot(aes(group = paste0(year, day)), color = "red", fill = NA, outlier.alpha = 0, width = 1)

serology_relevant_wide_eggcell_reponses <- serology_relevant_wide %>%
    select(-d0, -d14, -virus) %>%
    pivot_wider(names_from = "virus_egg_cell", values_from = "ratio") %>%
    mutate(ratio = cell / egg)

serology_relevant_wide_eggcell_reponses_plot <- serology_relevant_wide_eggcell_reponses %>%
    filter(!is.na(ratio)) %>%
    left_join(pid_jitters, "pid") %>%
    mutate(
        xcoord = as.integer(factor(year)) + pid_jitterx,
        ycoord = exp(log(ratio) + pid_jittery),
    ) %>%
    ggplot(aes(xcoord, ycoord)) +
    theme_bw() +
    theme(
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
    ) +
    scale_y_log10("Cell/egg response ratio", breaks = 2^(-10:10)) +
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("2020", "2021", "2022")) +
    coord_cartesian(ylim = c(2^(-8), 64)) +
    geom_hline(yintercept = 1, lty = "11", col = "gray50") +
    geom_point(alpha = 0.1, shape = 18, color = "gray50") +
    geom_boxplot(aes(group = paste0(year)), color = "red", fill = NA, outlier.alpha = 0, width = 1)

eggcell_titre_and_response_ratios_plot <- ggpubr::ggarrange(serology_relevant_wide_eggcell_plot, serology_relevant_wide_eggcell_reponses_plot, ncol = 1, align = "v")

ggsave("h1-eggcell/eggcell_ratio.pdf", eggcell_titre_and_response_ratios_plot, width = 15, height = 15, units = "cm")

corr_titre_plot <- serology_relevant_wide_eggcell %>%
    filter(!is.na(cell), !is.na(egg)) %>%
    left_join(pid_jitters, "pid") %>%
    mutate(
        xcoord = exp(log(cell) + pid_jitterx),
        ycoord = exp(log(egg) + pid_jittery),
    ) %>%
    ggplot(aes(xcoord, ycoord)) +
    theme_bw() +
    theme(
        panel.spacing = unit(0, "null"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
    ) +
    facet_grid(day ~ year) +
    scale_x_log10("Cell", breaks = 5 * 2^(0:15)) +
    scale_y_log10("Egg", breaks = 5 * 2^(0:15)) +
    geom_point(alpha = 0.1, shape = 18, color = "gray50") +
    geom_label(
        aes(label = paste0("cor = ", round(correlation, 2))), x = 3, y = 1,
        data = serology_relevant_wide_eggcell %>%
            filter(!is.na(cell), !is.na(egg)) %>%
            summarise(.by = c(year, day), correlation = cor(log(cell), log(egg)))
    )

corr_ratio_plot <- serology_relevant_wide_eggcell_reponses %>%
    filter(!is.na(cell), !is.na(egg)) %>%
    left_join(pid_jitters, "pid") %>%
    mutate(
        xcoord = exp(log(cell) + pid_jitterx),
        ycoord = exp(log(egg) + pid_jittery),
    ) %>%
    ggplot(aes(xcoord, ycoord)) +
    theme_bw() +
    theme(
        panel.spacing = unit(0, "null"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.minor = element_blank(),
    ) +
    facet_wrap(~year, nrow = 1) +
    scale_x_log10("Cell", breaks = 2^(0:15)) +
    scale_y_log10("Egg", breaks = 2^(0:15)) +
    geom_point(alpha = 0.1, shape = 18, color = "gray50") +
    geom_label(
        aes(label = paste0("cor = ", round(correlation, 2))), x = 2, y = 0,
        data = serology_relevant_wide_eggcell_reponses %>%
            filter(!is.na(cell), !is.na(egg)) %>%
            summarise(.by = year, correlation = cor(log(cell), log(egg)))
    )

corr_plots <- ggpubr::ggarrange(corr_titre_plot, corr_ratio_plot, ncol = 1, heights = c(2, 1))

ggsave("h1-eggcell/corr.pdf", corr_plots, width = 20, height = 20, units = "cm")

serology_relevant_wide_eggcell_reponses %>%
    select(-cell, -egg) %>%
    mutate(year = paste0("y", year)) %>%
    pivot_wider(names_from = "year", values_from = "ratio") %>%
    arrange(y2022) %>%
    write_csv("h1-eggcell/response_ratios.csv")
