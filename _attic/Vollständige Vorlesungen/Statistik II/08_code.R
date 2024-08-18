## ------------------------------------------------------------
## by J.Kruppa on Monday, June  7, 2021 (10:09)
pacman::p_load(tidyverse, plyr, broom, broom.mixed, magrittr,
               parameters, stargazer, geepack, datarium, lme4,
               janitor)
## ------------------------------------------------------------
load("dragons.RData")

dragons_tbl <- dragons %>%
  as_tibble %>%
  clean_names

hist(dragons_tbl$test_score)

## simple linear regression
lm_basic_fit <- lm(test_score ~ body_length, data = dragons_tbl)
summary(lm_basic_fit) 
lm_basic_fit %>% tidy()

##
ggplot(dragons_tbl, aes(x = body_length, y = test_score)) +
  geom_point() +
  geom_smooth(method = "lm") 

##
ggplot(dragons_tbl, aes(x = mountain_range, y = test_score)) +
  geom_boxplot() 

##
ggplot(dragons_tbl, aes(x = body_length, y = test_score,
                        colour = mountain_range)) +
  geom_point(size = 2) +
  theme_classic()##  +
    ## theme(legend.position = "none")

ggplot(dragons_tbl, aes(x = body_length, y = test_score,
                        colour = mountain_range)) +
  geom_point(size = 2) +
  facet_wrap( ~ mountain_range) +
  geom_smooth(method = "lm") +
  theme_bw()##  +
    ## theme(legend.position = "none")

## caclaulte separate regression on each mountain_range
lm_basic_fit_tbl <- ldply(levels(dragons_tbl$mountain_range), function(x) {
  tmp_tbl <- dragons_tbl %>%
    filter(mountain_range == x)
  lm(test_score ~ body_length, data = tmp_tbl) %>%
    tidy %>%
    mutate(mountain_range = x)
}, .progress = "text") %>%
  as_tibble %>%
filter(term != "(Intercept)")

## confounder adjustierung
lm_basic_fit <- lm(test_score ~ body_length + mountain_range, data = dragons_tbl)
summary(lm_basic_fit) 

## ------------------------------------------------------------
## by J.Kruppa on Monday, June  7, 2021 (16:13)
## mixed model

lmer_fit <- lmer(test_score ~ body_length + (1 | mountain_range),
                 data = dragons_tbl)

summary(lmer_fit)
lmer_fit %>% parameters()

## geht das nicht tidy?

lmer_fit_tbl <- lmer_fit %>% broom.mixed::tidy(conf.int = TRUE) %>%
  filter(effect == "fixed" & term != "(Intercept)") %>%
  select(term, estimate, conf.low, conf.high)

lmer_fit_pvalue_tbl <- lmer_fit %>% p_value

left_join(lmer_fit_tbl, lmer_fit_pvalue_tbl,
          by = c("term" = "Parameter"))

## Was soll eigentlich die 1 in (1 | mountain_range)
coef(lmer_fit)

lmer(test_score ~ (1 | mountain_range),
     data = dragons_tbl) %>% coef

dragons_tbl %>%
  group_by(mountain_range) %>%
  dplyr::summarise("mountain_range", mean(test_score))


lmer(test_score ~ body_length + (1 | mountain_range),
     data = dragons_tbl) %>% parameters()

## WRONG
lmer(test_score ~ body_length + (1 | mountain_range) + (1 | site),
     data = dragons_tbl) %>% summary

table(dragons_tbl$site)

## nested variable
dragons_tbl %<>%
  mutate(sample_chr = str_c(mountain_range, ":", site),
         sample_fct = as.factor(sample_chr))


lmer(test_score ~ body_length + (1 | mountain_range) + (1 | sample_fct),
     data = dragons_tbl) %>% summary

## in formelschreibweise
mixed_lmer2 <- lmer(test_score ~ body_length + (1 | mountain_range/site),
                    data = dragons_tbl)

mixed_lmer2 %>% summary

ggplot(dragons_tbl, aes(x = body_length, y = test_score, colour = site)) +
  facet_wrap(~ mountain_range, nrow=3) +
  geom_point() +
  theme_classic() +
  geom_line(data = cbind(dragons_tbl, pred = predict(mixed_lmer2)), aes(y = pred)) ## +
  ## theme(legend.position = "none")


##
gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
             data = cbpp, family = binomial)

gm1 %>% tidy(exponentiate = TRUE)


gm <- glm(cbind(incidence, size - incidence) ~ period,
             data = cbpp, family = binomial)

gm %>% tidy(exponentiate = TRUE)

with(cbpp, cbind(incidence, size - incidence))

dragons

data("selfesteem2", package = "datarium")

data(dietox)

ggplot(dietox, aes(Time, Weight)) + geom_point() +
  facet_wrap(~Litter)


## ------------------------------------------------------------
## by J.Kruppa on Monday, June  7, 2021 (10:23)
## Paired t-Test
## http://www.sthda.com/english/wiki/paired-samples-t-test-in-r

## Weight of the mice before treatment
before <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
## Weight of the mice after treatment
after <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
my_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  weight = c(before,  after)
)

within(dragons, sample <- factor(mountainRange:site))

mixed.lmer <- lmer(testScore ~ (1 |mountainRange), data = dragons)

coef(mixed.lmer)$mountainRange %>% round(1)

dragons %>%
  mutate(mountainRange = as.factor(mountainRange)) %>% 
  group_by(mountainRange) %>%
  dplyr::summarise(mean_test = round(mean(testScore), 1))
