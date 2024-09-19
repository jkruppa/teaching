## ------------------------------------------------------------
pacman::p_load(tidyverse, readxl, fs, ggridges,
               conflicted, see, janitor)
conflicts_prefer(purrr::discard)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::filter)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
## ------------------------------------------------------------

tibble(x = 0:10,
       y = 0:10) |> 
ggplot(aes(x, y)) +
  theme_minimal() + 
  labs(x = "", y = "",
       caption = "Erstellt mit {tidyverse} und {ggplot}") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray20"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "gray70")) 

ggsave(file.path("/Users/kruppajo/Movies/Statistik/sticker/empty_graph.png"),
       width = 14, height = 14, units = "cm")

tibble(x = 0:12,
       y = 0:12) |> 
  ggplot(aes(x, y)) +
  theme_minimal() + 
  xlim(0, 3) +
  ylim(0.5, 11.5) +
  geom_hline(yintercept = 11, size = 2) +
  geom_hline(yintercept = 1:10, colour = "gray50") +
  geom_vline(xintercept = 1.5, size = 2) +
  labs(x = "", y = "",
       caption = "Erstellt mit {tidyverse} und {ggplot}") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave(file.path("/Users/kruppajo/Movies/Statistik/sticker/empty_10x2_table.png"),
       width = 10, height = 14, units = "cm")


tibble(x = 0:12,
       y = 0:12) |> 
  ggplot(aes(x, y)) +
  theme_minimal() + 
  xlim(0, 22) +
  ylim(0.5, 11.5) +
  geom_hline(yintercept = 11, size = 2) +
  geom_hline(yintercept = 1:10, colour = "gray50") +
  geom_vline(xintercept = c(4, 8, 12, 16, 20)-1, size = 2) +
  labs(x = "", y = "",
       caption = "Erstellt mit {tidyverse} und {ggplot}") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave(file.path("/Users/kruppajo/Movies/Statistik/sticker/empty_10x6_table.png"),
       width = 24, height = 14, units = "cm")

tibble(x = 0:12,
       y = 0:12) |> 
  ggplot(aes(x, y)) +
  theme_minimal() + 
  xlim(0, 14) +
  ylim(0.5, 11.5) +
  geom_hline(yintercept = 11, size = 2) +
  geom_hline(yintercept = 1:10, colour = "gray50") +
  geom_vline(xintercept = c(4, 8, 12)-1, size = 2) +
  labs(x = "", y = "",
       caption = "Erstellt mit {tidyverse} und {ggplot}") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave(file.path("/Users/kruppajo/Movies/Statistik/sticker/empty_10x4_table.png"),
       width = 16, height = 14, units = "cm")

tibble(x = 0:12,
       y = 0:12) |> 
  ggplot(aes(x, y)) +
  theme_minimal() + 
  xlim(0, 12) +
  ylim(0, 12) +
  geom_hline(yintercept = c(1.25, 6, 10.75), size = 2) +
  geom_vline(xintercept = c(1.25, 6, 10.75), size = 2) +
  labs(x = "", y = "",
       caption = "Erstellt mit {tidyverse} und {ggplot}") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank()) 

ggsave(file.path("/Users/kruppajo/Movies/Statistik/sticker/empty_2x2_table.png"),
       width = 14, height = 14, units = "cm")


ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  stat_function(fun = dnorm, linewidth = 0.75, args = list(sd = 1)) +
  labs(x = "", y = "",
       caption = "Erstellt mit {tidyverse} und {ggplot}") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 14)) 

ggsave(file.path("/Users/kruppajo/Movies/Statistik/sticker/normal-distribution.png"),
       width = 14, height = 7, units = "cm")

ggplot(data.frame(x = c(0, 4)), aes(x)) +
  theme_minimal() +
  xlim(-0.1, 4) + ylim(-0.1, 4) +
  geom_segment(aes(x = 0, y = -0.1, xend = 0, yend = 4), linewidth = 1,
             arrow = arrow(length=unit(0.15,"cm"), ends="last", type = "closed")) +
  geom_segment(aes(x = -0.1, y = 0, xend = 4, yend = 0), linewidth = 1,
               arrow = arrow(length=unit(0.15,"cm"), ends="last", type = "closed")) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8)) 

ggsave(file.path("/Users/kruppajo/Movies/Statistik/sticker/xy-plot.png"),
       width = 10, height = 10, units = "cm")
