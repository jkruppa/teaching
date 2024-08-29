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
