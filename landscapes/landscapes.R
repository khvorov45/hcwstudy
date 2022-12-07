library(tidyverse)

vac_hist <- read_csv("data/vaccinations.csv", col_types = cols())

prior_vac_counts <- vac_hist %>%
    group_by(pid) %>%
    summarise(
        .groups = "drop",
        prior2020 = sum(year >= 2015 & year < 2020 & (status == "Australia" | status == "Overseas")),
        prior2021 = sum(year >= 2016 & year < 2021 & (status == "Australia" | status == "Overseas")),
        prior2022 = sum(year >= 2017 & year < 2022 & (status == "Australia" | status == "Overseas")),
    )

serlandscape_bare <- read_csv("data/serology-landscapes.csv", col_types = cols())

viruses <- serlandscape_bare %>%
    select(virus, virus_year, virus_number) %>%
    distinct() %>%
    arrange(virus_year) %>%
    mutate(virus = factor(virus) %>% fct_reorder(virus_year)) %>%
    arrange(virus) %>%
    select(-virus_year)

serlandscape <- serlandscape_bare %>%
    select(-virus) %>%
    left_join(viruses, "virus_number") %>%
    left_join(read_csv("data/participants.csv", col_types = cols()), "pid") %>%
    left_join(prior_vac_counts, "pid")



colnames(serlandscape)

titre_max <- max(serlandscape$titre)

unlink("landscapes/indiv-hi", recursive = TRUE)
dir.create("landscapes/indiv-hi")

indiv_plots <- serlandscape %>%
    # filter(pid == first(pid), year == first(year)) %>%
    mutate(day = factor(day)) %>%
    group_by(pid, year) %>%
    group_split() %>%
    map(function(onepid) {
        info <- onepid %>% select(pid, year, prior2020, prior2021, age = age_screening, gender, bmi) %>% distinct() %>% mutate(age = round(age), bmi = round(bmi))
        info_str <- paste(paste0(colnames(info), ": ", info), collapse = " | ")
        plot <- onepid %>%
            ggplot(aes(virus, titre, color = day, shape = day, lty = day)) +
            theme_bw() +
            theme(
                axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                axis.title.x = element_blank(),
                legend.position =  "bottom"
            ) +
            scale_color_manual("Day", breaks = c("0", "7", "14", "220"), values = viridis::viridis_pal(end = 0.7, option = "A")(4)) +
            scale_shape_manual("Day", breaks = c("0", "7", "14", "220"), values = c(15, 16, 17, 18)) +
            scale_linetype_manual("Day", breaks = c("0", "7", "14", "220"), values = c("solid", "91", "51", "11")) +
            scale_y_log10("Titre", breaks = 5 * 2^(0:20)) +
            coord_cartesian(ylim = c(5, titre_max)) +
            labs(caption = info_str) +
            geom_line(aes(group = paste0(pid, day))) +
            geom_point()
        attr(plot, "pidyear") <- paste(info$pid, info$year, sep = "_")
        plot
    })

walk(indiv_plots, function(pl) {
    pidyear <- attr(pl, "pidyear")
    ggsave(paste0("landscapes/indiv-hi/", pidyear, ".png"), pl, width = 22, height = 15, units = "cm")
})
