## ------------------------------------------------------------
## by J.Kruppa on Monday, April 12, 2021 (09:22)

## simStudy
pacman::p_load(simstudy, tidyverse, broom, broom.mixed,
               magrittr)

## Wiederholung glm

data(starwars)

starwars %<>%
  filter(mass < 1000)

starwars %>%
  filter(height < 150) %>%
  select(species, gender)

fit <- glm(mass ~ height, data = starwars, family = gaussian)

ggplot(starwars, aes(y = mass, x = height)) + geom_point() + 
  geom_smooth(method = 'lm')

glimpse(starwars)
  


## Rmarkdown für insider

