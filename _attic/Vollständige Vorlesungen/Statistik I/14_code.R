## ------------------------------------------------------------
## by J.Kruppa on Monday, February 15, 2021 (15:49)
pacman::p_load(tidyverse, broom, graphics)

## ------------------------------------------------------------
## by J.Kruppa on Monday, February 15, 2021 (15:49)

data_tbl <- read_rds("data_tbl.rds") %>%
  select(-id) %>%
  mutate(pain_bin_num = as.numeric(pain_bin) - 1)

## vollkommener Humbug 
lm(pain_bin_num ~ size, data = data_tbl) %>% summary()

pdf("test.pdf", 5, 5)
ggplot(data_tbl, aes(y = pain_bin_num, x = size)) +
  geom_point() +
  geom_smooth(method = "lm")
dev.off()

## logistische Regression (y = binär, x = kontenuierlich)

data_tbl <- read_rds("data_tbl.rds") %>%
  select(-id) %>%
  mutate(pain_bin_num = as.numeric(pain_bin) - 1)

## lm(size ~ skyscraper, data = data_tbl) %>% summary

fit_sum <- glm(pain_bin ~ size, data = data_tbl, family = binomial) %>% summary

## hässlich wie die Nacht
coefficients(fit_sum)[2,1] %>% exp

fit <- glm(pain_bin ~ size, data = data_tbl, family = binomial) 

## gebe mir  die Odds ratios und die KI wieder
fit %>% tidy(exponentiate = TRUE, conf.int = TRUE)

## logistische Regression (y = binär, x = binär)

pdf("test.pdf", 5, 5)
ggplot(data_tbl, aes(x = size_bin, y = pain_bin)) + geom_point()
dev.off()

pdf("test.pdf", 5, 5)
mosaicplot(size_bin ~ pain_bin, data_tbl)
dev.off()

glm(pain_bin ~ size_bin, data_tbl, family = binomial) %>%
  tidy(exponentiate = TRUE, conf.int = TRUE)
