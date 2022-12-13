## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
## install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,
               emmeans, multcomp)

## wir lesen immer nur ein Tabellenblatt ein
data_tbl <- read_excel("/Users/kruppajo/Documents/GitHub/teaching/Spielweise in R (Level 3)/01_soli_analysis/soil_ph_data.xlsx") %>% 
  clean_names() %>% 
  mutate(substrat = as_factor(substrat),
         ph = as_factor(ph))

ggplot(data_tbl, aes(x = substrat, y = diameter, fill = ph)) +
  geom_boxplot()

fit <- lm(freshmatter ~ substrat + ph + substrat:ph, data = data_tbl)

fit %>%  anova()

fit %>% 
  emmeans(~ substrat) %>% 
  cld(Letters = letters, adjust = "none")

fit %>% 
  emmeans(~ ph) %>% 
  cld(Letters = letters, adjust = "none")
