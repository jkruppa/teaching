## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
## install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,
               emmeans, multcomp, magrittr,
               parameters, effectsize,
               multcompView,
               conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("extract", "magrittr")

## wir lesen immer nur ein Tabellenblatt ein
variant_tbl <- read_excel("/Users/kruppajo/Documents/GitHub/teaching/Spielweise in R (Level 3)/02_tukeyhsd_analysis/02_tukeyhsd_analysis.xlsx") %>% 
  clean_names() %>% 
  mutate(variante = as_factor(variante))

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

## einmal mit TukeyHDS()

aov_fit <- aov(height ~ variante, data = variant_tbl)

tukey_obj <- aov_fit %>% 
  TukeyHSD()

tukey_obj %>% 
  pluck("variante") %>% 
  magrittr::extract(, "p adj") %>% 
  multcompLetters()







