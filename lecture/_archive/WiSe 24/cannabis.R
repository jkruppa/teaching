library(readxl)
library(tidyverse)
library(janitor)
library(see)

cannabis_tbl <- read_excel("Messdaten Gruppe Cannabis.xlsx", 
                           sheet = "Wöchentliche Messungen 2") %>% 
  clean_names() %>% 
  select(treatment, block, termin, pflanzenhohe_cm,
         anzahl_seitentriebe) %>% 
  filter(termin %in% 1:8) %>% 
  mutate(treatment = as_factor(treatment),
         block = as_factor(block),
         termin_fct = as_factor(termin))

ggplot(filter(cannabis_tbl, treatment %in% c("T2", "K")), 
       aes(x = termin, y = anzahl_seitentriebe,
                         color = treatment)) +
  theme_minimal() +
  #geom_point(position = position_dodge(0.5)) +
  stat_summary(fun = "mean", geom = "line",
               position = position_dodge(0)) +
  scale_color_okabeito() +
  scale_x_continuous(breaks = 1:8) +
  facet_wrap( ~ block)

ggplot(cannabis_tbl, aes(y = pflanzenhohe_cm, 
                         x = anzahl_seitentriebe,
                         color = treatment)) +
  theme_minimal() +
  geom_point() +
  scale_color_okabeito() +
  facet_wrap()
