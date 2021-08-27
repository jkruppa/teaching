## ------------------------------------------------------------
## by J.Kruppa on Monday, February  8, 2021 (15:23)
pacman::p_load(tidyverse, broom, mosaic, corrr)

snake_tbl <- tibble(snake_id = 1:6,
                    mass = c(6, 8, 5, 7, 9, 11),
                    population = c(1, 1, 2, 2, 3, 3),
                    field = c(1, 1, 1, 1, 2, 2),
                    length = c(40, 45, 39, 50, 52, 57))

## ------------------------------------------------------------
## y = kontenuierlich, x = binär (zwei Gruppen)

ggplot(snake_tbl, aes(field, mass)) + geom_point()  +
  lims(x = c(0, NA), y = c(0, NA)) 

t.test(mass ~ field, data = snake_tbl, var.equal = FALSE)

lm(mass ~ field, data = snake_tbl) %>% summary

lm(mass ~ field, data = snake_tbl) %>% tidy

lm(mass ~ field, data = snake_tbl) %>% 
  confint()

## mal mit as.factor

snake_fac_tbl <- snake_tbl %>%
  mutate(field = factor(field,
                        labels = c("berlin", "hamburg")))

pdf("test.pdf", height = 6, width = 6)
ggplot(snake_fac_tbl, aes(field, mass)) + geom_point() 
dev.off()

lm(mass ~ field, data = snake_fac_tbl) %>% summary

model.matrix(mass ~ field, data = snake_fac_tbl)

## mean parametrization
model.matrix(mass ~ 0 + field, data = snake_fac_tbl)

lm(mass ~ 0 + field, data = snake_fac_tbl) %>% summary


## ------------------------------------------------------------
## y = kontenuierlich, x = categorial (> 2 Gruppen)


n_category

cate_tbl <- tibble(x = factor(rep(1:3, each = 5),
                              labels = c("low", "mid", "high")),
                   y = c(rnorm(5, 5, 2),
                         rnorm(5, 5, 2),
                         rnorm(5, 15, 2)))

pdf("test.pdf", height = 6, width = 6)
ggplot(cate_tbl, aes(x, y)) + geom_point() 
dev.off()

lm(y ~ x, data = cate_tbl) %>% summary


## ------------------------------------------------------------
## Multiple lineare regression in R

jump_data <- read_csv("jump_length.csv") %>%
  sample_n(300) %>%
  mutate(gender = as.factor(gender),
         type = as.factor(type))

pdf("test.pdf", height = 6, width = 6)
ggplot(data = jump_data,
       aes(x = weight, y = jump_length,
           color = gender, shape = type)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()
dev.off()

lm(jump_length ~ weight, data = jump_data) %>%
  summary()

lm(jump_length ~ weight + gender, data = jump_data) %>%
  summary()

lm(jump_length ~ weight + gender + type + vitality, data = jump_data) %>%
  summary()


jump_num_data <- read_csv("jump_length.csv") %>%
  sample_n(300) %>%
  mutate(gender = as.factor(gender),
         type = as.factor(type)) %>%
  mutate_if(is.factor, as.numeric)


cor(jump_num_data)
cov(jump_num_data)
var(jump_num_data$gender)



## ------------------------------------------------------------
## by J.Kruppa on Monday, February  8, 2021 (13:19)
gandalf_str <- "You shall not pass!"
