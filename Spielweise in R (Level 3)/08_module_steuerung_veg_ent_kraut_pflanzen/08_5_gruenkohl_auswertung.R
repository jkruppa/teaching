pacman::p_load(tidyverse, readxl, report, emmeans,
               multcomp, zoo, see,  conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
cbb_pal <- c("#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", 
             "#CC79A7", "#999999", "#000000")

green_tbl <- read_excel("/Users/kruppajo/work/GitHub/teaching/Spielweise in R (Level 3)/08_module_steuerung_veg_ent_kraut_pflanzen/wachstum_gruenkohl.xlsx") %>% 
  mutate(light = as_factor(light),
         variety = as_factor(variety),
         outcome = as_factor(outcome))

## Höhe des Grünkohls

height_tbl <- green_tbl %>% 
  filter(outcome == "height") %>% 
  select(light, variety, outcome, drymatter, wetmatter)

wetmatter_fit <- lm(wetmatter ~ light + variety + light:variety,
                 data = height_tbl) 

wetmatter_fit %>% 
  anova() 

drymatter_fit <- lm(drymatter ~ light + variety + light:variety,
                    data = height_tbl) 

drymatter_fit %>% 
  anova() 

wetmatter_fit %>% 
  emmeans(~ light * variety, 
          vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters) %>% 
  ggplot(aes(x = light, y = emmean,
             fill = variety)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE),
                width = 0.2,  
                position = position_dodge(width = 0.9, preserve = "single")) +
  geom_text(aes(label = .group, y = emmean + SE + 0.01),  
            position = position_dodge(width = 0.9), vjust = -0.25) +
  theme_minimal() +
  labs(x = "Licht",
       y = "Frischgewicht in [g]",
       fill = "Sorte")

## Pseudo Zeitreihe

height_tbl <- green_tbl %>% 
  filter(outcome == "height") %>% 
  select(light, variety, outcome, `45393`:`45435`)

## Wie die Zeit richtig umwandeln?
height_long_tbl <- height_tbl %>% 
  pivot_longer(cols = `45393`:`45435`,
               values_to = "height",
               names_to = "time") %>% 
  mutate(time = as.numeric(time))

## Wie die Legende zusammensetzen?
height_long_tbl %>% 
  ggplot(aes(x = time, y = height,
             color = interaction(variety, light), linetype = interaction(variety, light))) +
  theme_minimal() +
  geom_point(position = position_dodge(0.9), show_guide = FALSE) +
  stat_smooth(method = "loess", se = FALSE) +
  labs(color = "name", linetype = "name") +
  scale_color_manual(name = "name", values = cbb_pal[c(1:3, 6)]) +
  scale_linetype_manual(name = "name", values = c(1, 3, 1, 3)) +
  theme(legend.key.width = unit(2,"line"))

height_long_tbl %>% 
  ggplot(aes(x = time, y = height, 
             shape = interaction(variety, light),
             color = interaction(variety, light), 
             linetype = interaction(variety, light))) +
  theme_minimal() +
  geom_point(position = position_dodge(0.9), size = 2) +
  stat_smooth(method = "loess", se = FALSE) +
  labs(color = "name", linetype = "name") +
  scale_color_manual(name = "name", values = cbb_pal[c(1:3, 6)]) +
  scale_linetype_manual(name = "name", values = c(1, 3, 1, 3)) +
  scale_shape_manual(name = "name", values = c(1:4)) +
  theme(legend.key.width = unit(2,"line"))

ggplot(data = mydf, aes(x = year, y = mean,
                        color = interaction(treatment, treatment.type),
                        linetype = interaction(treatment, treatment.type))) +
  geom_point(show.legend = FALSE) +
  geom_line() +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),width = 0.1, size = 0.5) #+
 # scale_color_manual(name = "Treatment conditions", values = rep(c("blue", "blue", "red", "red"), times = 2)) +
 # scale_linetype_manual(name = "Treatment conditions", values = rep(c(1,2), times = 4))

## Die Punkte nach Lichtstärke (Tab Light Data einfärben)

