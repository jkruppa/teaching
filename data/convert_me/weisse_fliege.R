pacman::p_load(tidyverse, readxl, emmeans,
               multcomp)

fly_tbl <- read_excel("weisse_fliege.xlsx", sheet = "hauptdaten")  %>% 
  separate(variante, c("trt", "rep")) %>% 
  mutate(gattung = as_factor(gattung),
         trt = as_factor(trt),
         rep = as_factor(rep),
         anzahl_pflanze = anzahl/6) 

ggplot(fly_tbl, aes(x = trt, y = anzahl,
                    color = rep)) +
  geom_point() +
  facet_wrap(~ gattung, scales = "free_y")

fuchsia_tbl <- fly_tbl %>% 
  filter(gattung == "Fuchsia")

fuchsia_lm <- lm(anzahl ~ trt, fuchsia_tbl)

fuchsia_lm %>% anova()

fuchsia_lm %>% 
  emmeans(~ trt, vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters) %>% 
  arrange(trt)

euphorbia_tbl <- fly_tbl %>% 
  filter(gattung == "Euphorbia") %>% 
  filter(anzahl < 300)

euphorbia_lm <- lm(anzahl ~ trt, euphorbia_tbl)

euphorbia_lm %>% anova()

euphorbia_lm %>% 
  emmeans(~ trt, vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters) %>% 
  arrange(trt)

gras_tbl <- read_excel("Ergebnisse_Hauptversuch_WiSe23_24.xlsx", 
                       sheet = "data") %>% 
  mutate(Behandlung = as_factor(Behandlung),
         Konzentration = as_factor(Konzentration),
         Wiederholung = as_factor(Wiederholung),
         Sorte = as_factor(Sorte),
         Regime = as_factor(Regime)) %>% 
  group_by(Behandlung, Konzentration,
           Sorte, Regime, Wiederholung) %>% 
  summarise(sum_gekeimt = sum(gekeimt))

lm(sum_gekeimt ~ Behandlung + Konzentration + Regime, data = gras_tbl) %>% 
  anova()

ggplot(gras_tbl, aes(Behandlung, sum_gekeimt,
                     fill = Konzentration)) +
  geom_boxplot() +
  facet_wrap(~ Regime)
  
ggplot(gras_tbl, aes(Konzentration, sum_gekeimt,
                     fill = Behandlung)) +
  geom_boxplot() +
  facet_wrap(~ Regime)

