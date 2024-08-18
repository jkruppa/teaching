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
variant_tbl <- read_excel("02_tukeyhsd_analysis.xlsx") %>% 
  clean_names() %>% 
  mutate(variante = as_factor(variante)) # %>% 
  # filter(freshweight >= 60)

variant_tbl %>%
  pull(variante) %>% 
  tabyl

## Abbildung machen
ggplot(variant_tbl, aes(x = variante, y = freshweight,
                        fill = variante, color = variante)) +
  # geom_boxplot() +
  # geom_jitter(width = 0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center") +
  theme_bw() +
  labs(x = "Substratvariante", y = "Frischgewicht in kg/h") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = -45, hjust=0))

## einmal mit TukeyHDS()

aov_fit <- aov(height ~ variante, data = variant_tbl)

aov_fit %>% summary

tukey_obj <- aov_fit %>% 
  TukeyHSD()

tukey_obj %>% 
  pluck("variante") %>% 
  extract(, "p adj") %>% 
  multcompLetters()










