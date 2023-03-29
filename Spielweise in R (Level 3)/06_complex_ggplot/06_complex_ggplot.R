## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
## install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,
               emmeans, multcomp, magrittr,
               parameters, effectsize,
               multcompView, see, 
               lme4, performance,
               conflicted)
conflicts_prefer(dplyr::filter)

#Daten einlesen
data_raw_tbl <- read_sav("lizard_estimation.sav")

data_tbl <- data_raw_tbl %>% 
  pivot_longer(cols = DE:Global,
               names_to = "country",
               values_to = "value") %>% 
  mutate(country = as_factor(country)) %>% 
  na.omit()

anno_tbl <- read_excel("additional_information.xlsx")

gg_temp <- ggplot() +
  theme_classic() +
  aes(x = country, y=value, fill = country) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(x = "")

p1 <- gg_temp %+%
  filter(data_tbl, country != "Global") +
  scale_y_continuous(name = "Geschätzte Anzahl der Eidechsenarten",
                     limits = c(0, 2000),
                     breaks = seq(0, 2000, 200), 
                     expand = c(0, 0)) +
  annotate("text", 
           x = 1:7, 
           y = c(300, 320, 300, NA, NA, 270, NA), 
           label = anno_tbl$Effektstärke[1:7])

p2 <- gg_temp %+%
  filter(data_tbl, country == "Global") +
  scale_y_continuous(name = "Geschätzte Anzahl der Eidechsenarten",
                     limits = c(0, 4000),
                     breaks = seq(0,4000,400), 
                     expand = c(0, 0),
                     position = "right")

p1 + p2 + 
  plot_layout(widths = c(6, 1))

