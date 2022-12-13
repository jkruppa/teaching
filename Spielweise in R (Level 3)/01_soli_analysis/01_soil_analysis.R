## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
## install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,
               emmeans, multcomp)

## wir lesen immer nur ein Tabellenblatt ein
data_tbl <- read_excel("Plagge_Ulrike.xlsx") %>% 
  clean_names() %>% 
  mutate(substrat = as_factor(substrat),
         variante = as_factor(variante),
         ph = as_factor(ph))

ggplot(data_tbl, aes(x = substrat, y = durchmesser, fill = ph)) +
  geom_boxplot()

fit <- lm(durchmesser ~ substrat + ph + substrat:ph, data = data_tbl)

fit %>%  anova()

fit %>% 
  emmeans(~ substrat) %>% 
  cld(Letters = letters, adjust = "none")

fit %>% 
  emmeans(~ ph) %>% 
  cld(Letters = letters, adjust = "none")
