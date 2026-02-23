pacman::p_load(tidyverse, ggimage)


get_front_matter <- function(type, title, title_col, date, filename, scale = c(1, 1)) {
  transparent <- function(img) {
    magick::image_fx(img, expression = "0.65*a", channel = "alpha")
  }
  
  p_raw <- ggplot() +
    theme_void() +
    ##theme_minimal() +
    coord_cartesian(xlim = c(0, 16), ylim = c(0, 9)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    geom_image(aes(x = 13, y = 2.5, image = "/Users/kruppajo/writing/_talks/front_back_matter/cover.png"), 
               size = 0.7, image_fun = transparent) +
    annotate("text", hjust = "left", size = 10, color = "gray50", x = 1, y = 7.5,
             label = type, fontface = 2) +
    annotate("text", hjust = "left", size = 3, color = "gray50", x = 1, y = 2,
             label = "Jochen Kruppa-Scheetz",
             fontface = 4) +
    annotate("text", hjust = "left", size = 3, color = "gray50", x = 1, y = 1.5,
             label = "Professor für Bio Data Science", fontface = 4) +
    annotate("text", hjust = "left", size = 3, color = "gray50", x = 1, y = 1,
             label = date, fontface = 4) 
  if(length(title) == 1) {
    p_raw +
      annotate("text", hjust = "left", size = 15*scale[1], color = title_col, x = 1, y = 5,
               label = title, fontface = 2)
  } else {
    p_raw +
      annotate("text", hjust = "left", size = 15*scale[1], color = title_col, x = 1, y = 5.75,
               label = title[1], fontface = 2) +
      annotate("text", hjust = "left", size = 15*scale[2], color = title_col, x = 1, y = 4.25,
               label = title[2], fontface = 2)
  }
  ggsave(filename,
         width = 1920, height = 1080, units = "px", dpi = 320, bg = "white")
}


get_back_matter <- function(filename){
  transparent <- function(img) {
    magick::image_fx(img, expression = "0.75*a", channel = "alpha")
  }
  
  ggplot() +
    theme_void() +
    ##theme_minimal() +
    coord_cartesian(xlim = c(0, 16), ylim = c(0, 9)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    geom_image(aes(x = 12.25, y = 3.5, image = "/Users/kruppajo/writing/_talks/front_back_matter/cover.png"), 
               size = 1.1, image_fun = transparent) +
    annotate("text", hjust = "left", size = 9, color = "gray50", x = 1, y = 8,
             label = "Weitere Informationen", fontface = 2) +
    annotate("text", hjust = "left", size = 4.5, color = "#ae1116", x = 1, y = 6.75,
             label = "YouTube @BioDataScience", fontface = 2) +
    annotate("text", hjust = "left", size = 4.5, color = "#ae1116", x = 1, y = 6,
             label = "www.linkedin.com/in/j-kruppa-scheetz", fontface = 2) +
    annotate("text", hjust = "left", size = 4.5, color = "#ae1116", x = 1, y = 5.25,
             label = "https://github.com/jkruppa/", fontface = 2) +
    annotate("text", hjust = "left", size = 4.5, color = "#ae1116", x = 1, y = 4.5,
             label = "https://www.hs-osnabrueck.de/prof-dr-jochen-kruppa-scheetz/", fontface = 2) +
    annotate("text", hjust = "left", size = 4.5, color = "#ae1116", x = 1, y = 3.75,
             label = "Openbook Bio Data Science: https://jkruppa.github.io/", fontface = 2) +
    annotate("text", hjust = "left", size = 3, color = "gray50", x = 1, y = 2,
             label = "Prof. Dr. Jochen Kruppa-Scheetz", fontface = 4) +
    annotate("text", hjust = "left", size = 3, color = "gray50", x = 1, y = 1.5,
             label = "Osnabrück University of Applied Sciences", fontface = 4)  +
    annotate("text", hjust = "left", size = 3, color = "gray50", x = 1, y = 1,
             label = "j.kruppa@hs-osnabrueck.de", fontface = 4) 
  ggsave(filename,
         width = 1920, height = 1080, units = "px", dpi = 320, bg = "white")
}  
  
get_front_matter(type = "Vortrag", title = c("Abenteuer Statistiklehre", "Vom Mythos der Improvisation"),
                 title_col = "#ae1116", date = "26. Februar 2026", scale = c(0.7, 0.6),
                 filename = file.path("/Users/kruppajo/writing/_talks/front_back_matter/front_page.png"))

get_back_matter(filename = file.path("/Users/kruppajo/writing/_talks/front_back_matter/back_page.png"))
  