## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
## install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,
               emmeans, multcomp, 
               parameters, effectsize,
               conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

## wir lesen immer nur ein Tabellenblatt ein
data_tbl <- read_excel("soil_ph_data.xlsx") %>% 
  clean_names() %>% 
  select(-grade) %>% 
  mutate(substrat = as_factor(substrat),
         ph = as_factor(ph))

## Abbildung machen
ggplot(data_tbl, aes(x = substrat, y = freshmatter, fill = ph)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Substratvariante", y = "Frischgewicht in kg/h",
       fill = "pH-Wert")

## lineares Modell
fit <- lm(freshmatter ~ substrat + ph + substrat:ph, data = data_tbl)

fit %>% 
  anova() %>% 
  model_parameters()

fit %>% 
  eta_squared()

## Paarweise Vergleiche

fit %>% 
  emmeans(~ substrat) %>% 
  cld(Letters = letters, adjust = "none") %>% ## bonferroni
  arrange(substrat)

fit %>% 
  emmeans(~ substrat) %>%   
  contrast(method = "pairwise", adjust = "bonferroni") %>% 
  as_tibble() %>% 
  arrange(p.value)

fit %>% 
  emmeans(~ ph) %>% 
  cld(Letters = letters, adjust = "none")

## das ganze mit interaktion

fit %>% 
  emmeans(~ substrat | ph) %>% 
  cld(Letters = letters, adjust = "none") ## bonferroni

fit %>% 
  emmeans(~ substrat | ph) %>%   
  contrast(method = "pairwise", adjust = "none") %>% 
  as_tibble() %>% 
  filter(ph == "high") %>% 
  arrange(p.value)









