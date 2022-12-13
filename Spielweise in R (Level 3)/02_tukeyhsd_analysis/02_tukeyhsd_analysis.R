## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
## install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,
               emmeans, multcomp, magrittr,
               parameters, effectsize,
               conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("extract", "magrittr")

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

aov_fit <- aov(height ~ variante, data = soil_tbl)

tukey_obj <- aov_fit %>% 
  TukeyHSD()

tukey_obj %>% 
  pluck("variante") %>% 
  magrittr::extract( , "p adj") %>% 
  multcompLetters()







