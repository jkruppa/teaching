## ------------------------------------------------------------
## by J.Kruppa on Monday, April 19, 2021 (09:34)
pacman::p_load(tidyverse, magrittr, mvtnorm, plyr, simstudy,
               broom, mosaic)

## Siimple gaussian linear regression

## Read SPSS wirth read_sav
easyFit_tbl <- read_csv("easyFit.csv") %>%
  mutate(weight_bin = factor(weight_bin, levels = c("normal", "critical")),
         easyFit = factor(easyFit, levels = c("placebo", "dose25", "dose50")))

## univariat
## Y = weight, X ist der Rest

## OLS = lm()
lm(weight ~ age, data = easyFit_tbl)

risk_vec <- names(easyFit_tbl) %>%
  str_subset("weight", negate = TRUE) ## %>%
  ## dput

lm_univariate_res_tbl <- ldply(risk_vec, function(x) {
  formula_vec <- reformulate(termlabels = x,
                             response = "weight")
  lm_fit <- lm(formula_vec, data = easyFit_tbl)
  lm_tidy <- lm_fit %>%
    tidy(conf.int = TRUE) %>%
    magrittr::extract(-1,)
  return(lm_tidy)
}, .progress = "text") %>%
  as_tibble %>%
select(-statistic) ## %>%
## kable

## Simple logistic linear regression

## glm()
glm(weight ~ age, data = easyFit_tbl, family = "gaussian")


glm_univariate_res_tbl <- ldply(risk_vec, function(x) {
  formula_vec <- reformulate(termlabels = x,
                             response = "weight_bin")
  glm_fit <- glm(formula_vec, data = easyFit_tbl, family = binomial)
  glm_tidy <- glm_fit %>%
    tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    magrittr::extract(-1,)
  return(glm_tidy)
}, .progress = "text") %>%
  as_tibble %>%
select(-statistic) ## %>%
## kable

## Multiple linear regression

## lm() - confounder
lm(weight ~ easyFit + gender + creatinin + age, data = easyFit_tbl) %>%
  summary

ggplot(easyFit_tbl, aes(easyFit, weight)) +
  geom_boxplot() +
  geom_jitter()

mosaic::mean(weight ~ easyFit, data = easyFit_tbl)

## lm() - interaction
lm(weight ~ easyFit + gender, data = easyFit_tbl) %>%
  summary

model.matrix(weight ~ easyFit + gender, data = easyFit_tbl)[1:10, ]

## 1. Patiente (mann und placebo)
easyFit_tbl %>% magrittr::extract(1,) %>% select(easyFit, gender)

lm(weight ~ easyFit + gender + easyFit:gender, data = easyFit_tbl) %>%
  summary

model.matrix(weight ~ easyFit + age + easyFit:age, data = easyFit_tbl)[1:10, ]


ggplot(easyFit_tbl, aes(easyFit, weight, fill = gender)) + geom_boxplot()


## multiple logistic linear regression

## glm() - confounder
glm(weight_bin ~ easyFit + creatinin + age + easyFit:age,
    data = filter(easyFit_tbl, gender != "man"),
    family = binomial) %>%
  tidy(exponentiate = TRUE) %>%
  as.data.frame

table(easyFit_tbl$weight_bin, easyFit_tbl$gender)



gandalf <- 1 
## You shall not pass

