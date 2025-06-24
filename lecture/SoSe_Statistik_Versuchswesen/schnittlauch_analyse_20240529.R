library(tidyverse)
library(readxl)
library(report)
library(emmeans)
library(multcomp)
library(conflicted)
library(zoo)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

schnitt_tbl <- read_excel("C:/Users/jokruppa/Desktop/schnittlauch_data.xlsx") %>% 
  mutate(sorte = as_factor(sorte),
         year = as_factor(year)) %>% 
  select(-id) %>% 
  pivot_longer(cols = `45411`:`45436`,
               values_to = "height",
               names_to = "day") %>% 
  mutate(day_date = as.Date(as.numeric(day), origin = "1899-12-30"))

## Höhe des Schnittlauchs

height_tbl <- schnitt_tbl %>% 
  filter(day_date == "2024-05-24")

height_fit <- lm(height ~ sorte + year + sorte:year,
                 data = height_tbl) 

height_fit %>% 
  anova() 

height_fit %>% 
  emmeans(~ sorte * year, 
          vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters) %>% 
  ggplot(aes(x = year, y = emmean,
             fill = sorte)) +
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

## Wie die Legende zusammensetzen?
schnitt_tbl %>% 
  ggplot(aes(x = day_date, y = height)) +
  theme_minimal() +
  geom_point(aes(color = year), position = position_dodge(0.9)) +
  geom_smooth(aes(linetype = sorte), method = "loess", se = FALSE) +
  labs(color = "name", linetype = "name")

## Die Punkte nach Lichtstärke (Tab Light Data einfärben)

temp_tbl <- read_excel("C:/Users/jokruppa/Desktop/Temperatur_Daten.xlsx") %>% 
  mutate(Uhrzeit = format(Uhrzeit, format = "%H:%M:%S"),
         Datum = format(Datum, format = "%Y-%m-%d"),
         datum_join = as.Date(Datum),
         datum_x = ymd(Datum) + hms(Uhrzeit))


left_join(temp_tbl,schnitt_tbl, by = join_by("datum_join" == "day_date"))

temp_tbl %>% 
  ggplot(aes(x = datum_x, y = Messwerte)) +
  geom_line()
  
ggplot() +
  theme_minimal() +
  # geom_point(position = position_dodge(0.9)) +
  stat_smooth(data = schnitt_tbl,
              aes(x = day_date, y = height,
                  color = year, linetype = sorte),
              method = "loess", se = FALSE) +
  geom_line(data = temp_tbl,
            aes(x = Datum, y = Messwerte))


