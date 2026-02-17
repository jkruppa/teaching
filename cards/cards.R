pacman::p_load(tidyverse, ggimage)

semester <- "Sommersemester 2026"

get_card <- function(type, title, title_col, semester, filename){
  p_raw <- ggplot() +
    theme_void() +
    ##theme_minimal() +
    coord_cartesian(xlim = c(0, 16), ylim = c(0, 9)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    geom_image(aes(x = 8, y = 1, image = "/Users/kruppajo/work/GitHub/teaching/cards/avatare_scene.png"), 
               size = 1.5) +
    annotate("text", hjust = "left", size = 10, color = "gray50", x = 1, y = 7.5,
             label = type,
             fontface = 2) +
    annotate("text", hjust = "right", size = 3, color = "gray50", x = 15, y = 3,
             label = "Prof. Dr. Jochen Kruppa-Scheetz",
             fontface = 4) +
    annotate("text", hjust = "right", size = 3, color = "gray50", x = 15, y = 2.5,
             label = semester,
             fontface = 2) 
  if(length(title) == 1) {
    p_raw +
      annotate("text", hjust = "left", size = 12.5, color = title_col, x = 1, y = 5,
               label = title, fontface = 2)
  } else {
    p_raw +
      annotate("text", hjust = "left", size = 12.5, color = title_col, x = 1, y = 5.75,
               label = title[1], fontface = 2) +
      annotate("text", hjust = "left", size = 12.5, color = title_col, x = 1, y = 4.25,
               label = title[2], fontface = 2)
  }
  ggsave(filename, width = 1920, height = 1080, units = "px", dpi = 320, bg = "white")
}

semester <- "Sommersemester 2026"

get_card(type = "Blockwoche", title = c("Einfach sprechen"),
         title_col = "#ae1116", semester = semester,
         filename = file.path("/Users/kruppajo/work/GitHub/teaching/cards/blockwoche.png"))

semester <- "Wintersemester 2026/27"

get_card(type = "Vorlesung", title = c("Mathematik & Statistik"),
         title_col = "#ae1116", semester = semester,
         filename = file.path("/Users/kruppajo/work/GitHub/teaching/cards/mathematik_statistik.png"))

get_card(type = "Vorlesung", title = c("Spezielle Statistik", "und Versuchswesen"),
         title_col = "#ae1116", semester = semester,
         filename = file.path("/Users/kruppajo/work/GitHub/teaching/cards/spezielle_statistik.png"))

get_card(type = "Vorlesung", title = c("Statistik"),
         title_col = "#ae1116", semester = semester,
         filename = file.path("/Users/kruppajo/work/GitHub/teaching/cards/statistik.png"))

semester <- "Sommersemester 2027"

get_card(type = "Vorlesung", title = c("Angewandte", "Mathematik & Statistik"),
         title_col = "#ae1116", semester = semester,
         filename = file.path("/Users/kruppajo/work/GitHub/teaching/cards/angewandte_mathematik_statistik.png"))

get_card(type = "Vorlesung", title = c("Angewandte Statistik", "und Versuchswesen"),
         title_col = "#ae1116", semester = semester,
         filename = file.path("/Users/kruppajo/work/GitHub/teaching/cards/angewandte_statistik_versuchswesen.png"))

get_card(type = "Vorlesung", title = c("Biostatistik"),
         title_col = "#ae1116", semester = semester,
         filename = file.path("/Users/kruppajo/work/GitHub/teaching/cards/biostatistik.png"))

get_card(type = "Vorlesung", title = c("Modellierung landwirt-", "schaftlicher Daten"),
         title_col = "#ae1116", semester = semester,
         filename = file.path("/Users/kruppajo/work/GitHub/teaching/cards/modellierung_landwirtschaftlicher_daten.png"))



