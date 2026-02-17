pacman::p_load(tidyverse, ggimage)

semester <- "Sommersemester 2026"

ggplot() +
  theme_void() +
  ##theme_minimal() +
  coord_cartesian(xlim = c(0, 16), ylim = c(0, 9)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  geom_image(aes(x = 8, y = 1, image = "/Users/kruppajo/work/GitHub/teaching/cards/avatare_scene.png"), 
             size = 1.5) +
  annotate("text", hjust = "left", size = 10, color = "gray50", x = 1, y = 7.5,
           label = "Vorlesung",
           fontface = 2) +
  annotate("text", hjust = "left", size = 15, color = "#ae1116", x = 1, y = 5.25,
           label = "Biostatistik",
           fontface = 2) +
  annotate("text", hjust = "right", size = 3, color = "gray50", x = 15, y = 3.25,
           label = "Prof. Dr. Jochen Kruppa-Scheetz",
           fontface = 4) +
  annotate("text", hjust = "right", size = 3, color = "gray50", x = 15, y = 2.75,
           label = semester,
           fontface = 2) 

ggsave(file.path("/Users/kruppajo/work/GitHub/teaching/cards/test.png"),
       width = 1920, height = 1080, units = "px", dpi = 320, bg = "white")

