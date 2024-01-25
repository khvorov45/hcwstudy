library(tidyverse)

read_csv("mncorr/MN_B_Washington_2020_2021.csv", col_types = cols()) %>%
    mutate(day = str_replace(TP, "V", "") %>% as.numeric()) %>%
    select(pid = ID, year = Year, day, mn = MN_Titre) %>%
    left_join(
        read_csv("data/serology.csv", col_types = cols()) %>%
            filter(virus == "B/Washington/02/2019e", vax_inf == "V") %>%
            select(pid, year, day, titre),
        c("pid", "year", "day"),
    ) %>%
    (function(data) {
        cor_result <- cor.test(log(data$titre), log(data$mn))
        data %>%
            ggplot(aes(titre, mn)) +
            theme_bw() +
            theme(
                panel.grid.minor = element_blank(),
            ) +
            coord_cartesian(ylim = c(5, 5120), xlim = c(5, 5120)) +
            scale_x_log10("HI", breaks = 5 * 2^(0:20)) +
            scale_y_log10("MN", breaks = 5 * 2^(0:20)) +
            geom_point(alpha = 0.5, shape = 18) +
            geom_text(
                aes(x = 1280, y = 8), label = glue::glue("cor {round(cor_result$estimate, 2)} ({round(cor_result$conf.int[1], 2)}, {round(cor_result$conf.int[2], 2)})"), 
                inherit.aes = FALSE, data = tibble()
            ) +
            geom_text(
                aes(x = 1280, y = 5), label = glue::glue("p={signif(cor_result$p.value, 2)}"), 
                inherit.aes = FALSE, data = tibble()
            )
    }) %>%
    ggsave("mncorr/washington.pdf", ., width = 10, height = 10, units = "cm")
