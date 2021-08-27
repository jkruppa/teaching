## ------------------------------------------------------------
## by J.Kruppa on Monday, January 25, 2021 (14:39)
pacman::p_load(tidyverse, broom, mosaic)

snake_tbl <- tibble(snake_id = 1:6,
                    mass = c(6, 8, 5, 7, 9, 11),
                    population = c(1, 1, 2, 2, 3, 3),
                    field = c(1, 1, 1, 1, 2, 2),
                    length = c(40, 45, 39, 50, 52, 57))

lm_fit <- lm(mass ~ length, data = snake_tbl)

summary(lm_fit)

model.matrix(mass ~ length, data = snake_tbl)

residuals(lm_fit)
fitted(lm_fit)

cor(mass ~ length, data = snake_tbl)
cor(mass ~ length, data = snake_tbl)^2



xy_tbl <- tibble(x = rnorm(n = 100, 5, 2),
                 y = rnorm(n = 100, 5, 2))

ggplot(xy_tbl, aes(x, y)) + geom_point() +
  lims(x = c(0, NA), y = c(0, NA)) +
  stat_smooth(method = "lm", col = "red")

cov(xy_tbl$x, xy_tbl$y)

cor(xy_tbl$x, xy_tbl$y)



xy_tbl <- tibble(x = rpois(n = 100, lambda = 5),
                 y = 5 + 0.25 * x + rnorm(100, 0, 8))

ggplot(xy_tbl, aes(x, y)) + geom_point() +
  lims(x = c(0, NA), y = c(0, NA)) +
  stat_smooth(method = "lm", col = "red")

lm(y ~ x, data = xy_tbl) %>% summary


## y = kontenuierlich, x = binär (zwei Gruppen)

ggplot(snake_tbl, aes(field, mass)) + geom_point() ## +
  ## lims(x = c(0, NA), y = c(0, NA)) 

t.test(mass ~ field, data = snake_tbl, var.equal = TRUE)

lm(mass ~ field, data = snake_tbl) %>% summary

lm(mass ~ field, data = snake_tbl) %>% tidy

## mal mit as.factor

snake_fac_tbl <- snake_tbl %>%
  mutate(field = factor(field, labels = c("sea", "land")))

ggplot(snake_fac_tbl, aes(field, mass)) + geom_point() 

lm(mass ~ field, data = snake_fac_tbl) %>% summary

model.matrix(mass ~ field, data = snake_fac_tbl)


gandalf_str <- "You shall not pass!"



















lm(mass ~ field, data = data_tbl) %>%
  tidy %>%
  filter(term == "field") %>% 
  select(estimate, statistic, p.value)
t.test(mass ~ field, data = data_tbl, var.equal = TRUE) %>% tidy %>%
  select(estimate, statistic, p.value)

model.matrix(mass ~ field, data = data_tbl)

mosaic::mean(data_tbl$mass)

lm(mass ~ 1, data = data_tbl)
model.matrix(mass ~ 1, data = data_tbl)
