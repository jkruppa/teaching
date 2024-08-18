## ------------------------------------------------------------
## by J.Kruppa on Monday, January 25, 2021 (14:39)
pacman::p_load(tidyverse, broom, mosaic)


snake_tbl <- tibble(snake_id = 1:6,
                    mass = c(6, 8, 5, 7, 9, 11),
                    population = c(1, 1, 2, 2, 3, 3),
                    field = c(1, 1, 1, 1, 2, 2),
                    length = c(40, 45, 39, 50, 52, 57))

mean(snake_tbl$mass)

## nur intercept
lm(mass ~ 1, data = snake_tbl)

ggplot(snake_tbl, aes(x = snake_id, y = mass)) +
  geom_point() +
  geom_hline(yintercept = 7.6666)

## y = kontenuierlich, x = kontenuierlich

ggplot(data = snake_tbl, aes(x = length, y = mass)) +
  geom_point()

lm(mass ~ length, data = snake_tbl) %>%  summary

lm(mass ~ length, data = snake_tbl) %>%  tidy

