pacman::p_load(tidyverse, readxl, report, emmeans,
               multcomp, conflicted)
conflicts_prefer(dplyr::select)

basi_tbl <- read_excel("/Users/kruppajo/work/GitHub/teaching/Spielweise in R (Level 3)/08_module_steuerung_veg_ent_kraut_pflanzen/keimversuch_basilikum_2.xlsx", 
                        sheet = "rdata") %>% 
  mutate(versuchsgruppe = as_factor(versuchsgruppe))


## Stängeldurchmesser

staengel_tbl <- basi_tbl %>% 
  select(versuchsgruppe, t2_stängel, t3_stängel, t4_stängel)

staengel_long_tbl <- staengel_tbl %>% 
  pivot_longer(cols = t2_stängel:t4_stängel,
               values_to = "durchmesser",
               names_to = c("time", "outcome"),
               names_sep = "_") %>% 
  mutate(time = factor(time, 
                       labels = c("29.04.2024", "06.05.2024",	"13.05.2024")))

staengel_fit <- lm(durchmesser ~ versuchsgruppe + time + versuchsgruppe:time,
   data = staengel_long_tbl) 

staengel_fit %>% 
  anova() %>% 
  report()

## pro zeitpunkt
staengel_fit %>% 
  emmeans(~ versuchsgruppe | time, 
          vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters)

## über alle zeitpunkte
staengel_fit %>% 
  emmeans(~ versuchsgruppe * time, 
          vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters)

## einmal als visualisierung
staengel_fit %>% 
  emmeans(~ versuchsgruppe | time, 
          vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters) %>% 
  ggplot(aes(x = time, y = emmean,
             fill = versuchsgruppe)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE),
                width = 0.2,  
                position = position_dodge(width = 0.9, preserve = "single")) +
  geom_text(aes(label = .group, y = emmean + SE + 0.01),  
            position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_minimal() +
  labs(x = "Zeitpunkt der Messung",
       y = "Mittlerer Stängeldurchmesser in [mm]",
       fill = "Versuchsgruppe")

## Stängeldurchmesser

keimrate_tbl <- basi_tbl %>% 
  select(versuchsgruppe, t1_keimrate, t2_keimrate, t3_keimrate)

keimrate_long_tbl <- keimrate_tbl %>% 
  pivot_longer(cols = t1_keimrate:t3_keimrate,
               values_to = "keimrate",
               names_to = c("time", "outcome"),
               names_sep = "_") %>% 
  mutate(time = factor(time, 
                       labels = c("29.04.2024", "06.05.2024",	"13.05.2024")))

keimrate_fit <- lm(keimrate ~ versuchsgruppe + time + versuchsgruppe:time,
                   data = keimrate_long_tbl) 

keimrate_fit %>% 
  anova() %>% 
  report()

## pro zeitpunkt
keimrate_fit %>% 
  emmeans(~ versuchsgruppe | time, 
          vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters)

## über alle zeitpunkte
keimrate_fit %>% 
  emmeans(~ versuchsgruppe * time, 
          vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters)

## einmal als visualisierung
keimrate_fit %>% 
  emmeans(~ versuchsgruppe | time, 
          vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters) %>% 
  ggplot(aes(x = time, y = emmean,
             fill = versuchsgruppe)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE),
                width = 0.2,  
                position = position_dodge(width = 0.9, preserve = "single")) +
  geom_text(aes(label = .group, y = emmean + SE + 0.01),  
            position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_minimal() +
  labs(x = "Zeitpunkt der Messung",
       y = "Mittlere Keimrate in [%]",
       fill = "Versuchsgruppe")

keimrate_long_tbl %>% 
  ggplot(aes(x = time, y = keimrate,
             color = versuchsgruppe, group = versuchsgruppe)) +
  geom_point(position = position_dodge(0.9)) +
  ylim(c(50, 100)) +
  stat_smooth(method = "loess", se = FALSE,
              position = position_dodge(0.9))


keimrate_staengel_tbl <- basi_tbl %>% 
  select(versuchsgruppe, t3_keimrate, t3_stängel)

keimrate_staengel_tbl %>% 
  ggplot(aes(x = t3_keimrate, y = t3_stängel,
             color = versuchsgruppe)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

keimrate_staengel_tbl <- basi_tbl %>% 
  select(versuchsgruppe, t2_keimrate, t2_stängel)

keimrate_staengel_tbl %>% 
  ggplot(aes(x = t2_keimrate, y = t2_stängel,
             color = versuchsgruppe)) +
  geom_point() +
  stat_smooth(method = "loess", se = FALSE)
