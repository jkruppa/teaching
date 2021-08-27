## ------------------------------------------------------------
## by J.Kruppa on Monday, May 10, 2021 (08:32)
pacman::p_load(tidyverse, plyr, car, rcompanion, lmtest,
               olsrr, ggdag, dagitty, janitor, tableone,
               broom)

data_dir <- file.path("C:\\Users\\kruppajo\\Desktop\\20210510")
easyfit_file <- file.path(data_dir, "easyFit.csv")
birds_file <- file.path(data_dir, "birds.csv")
longnose_file <- file.path(data_dir, "longnose.csv")

## ------------------------------------------------------------
## by J.Kruppa on Monday, May 10, 2021 (14:34)

longnose_tbl <- read_csv(longnose_file) %>%
  clean_names %>%
  mutate(stream = tolower(stream),
         longnose_log = log(longnose)) %>%
  select(-longnose, -stream)

## was ist das beste Modell?

## formula_fit <- reformulate(termlabels = names(longnose_tbl),
##                            response = "longnose_log")

null_fit <- glm(longnose_log ~ 1, data = longnose_tbl, family = gaussian)

full_fit <- glm(longnose_log ~ acerage + do2 + maxdepth +
                  no3 + so4 + temp, data = longnose_tbl, family = gaussian)

risk_vec <- c("acerage", "do2", "maxdepth", "no3", "so4", "temp")

univariat_fit_tbl <- ldply(risk_vec, function(x) {
  tmp_tbl <- longnose_tbl %>%
    select(longnose_log, x)
  tmp_formula <- reformulate(termlabels = x,
                             response = "longnose_log")
  tmp_fit <- glm(tmp_formula, data = tmp_tbl, family = gaussian) %>%
    tidy %>%
    magrittr::extract(-1,)
  return(tmp_fit)
}) %>% as_tibble

selected_risk_vec <- univariat_fit_tbl %>%
  filter(p.value <= 0.05) %>%
  pull(term)

selected_formula <- reformulate(termlabels = selected_risk_vec,
                                response = "longnose_log")

glm(selected_formula, data = longnose_tbl, family = gaussian)  %>%
  summary


## warum nicht alles
model <- lm(longnose_log ~ acerage + do2 + maxdepth +
              no3 + so4 + temp, data = longnose_tbl)

ols_step_all_possible(model) %>%
  as_tibble %>%
  arrange(desc(adjr)) %>%
  filter(n <= 4) %>% 
  select(predictors, adjr, aic) 

k <- ols_step_all_possible(model)
plot(k)

## test model selction

model.1 <- lm(longnose_log ~ acerage,                          data = longnose_tbl)
model.2 <- lm(longnose_log ~ maxdepth,                         data=longnose_tbl)
model.3 <- lm(longnose_log ~ no3,                              data=longnose_tbl)
model.8 <- lm(longnose_log ~ acerage  + maxdepth + no3 + do2, data=longnose_tbl)               
model.9 <- lm(longnose_log ~ acerage  + maxdepth + no3 + so4, data=longnose_tbl)                      
model.10 <- lm(longnose_log ~ acerage  + maxdepth + no3 + temp, data=longnose_tbl)               
               
library(rcompanion)

## was ist mein bestes Modell nach AIC, adjRS
compareLM(model.1, model.2, model.3, model.9, model.10)

## modelle vergleichen

anova(model.9, model.10) ## wenn DF gleich ist, geht es nicht...

lrtest(model.9, model.10)

## EPV 

(nrow(longnose_tbl)/7) %>% floor
(nrow(longnose_tbl)/15) %>% floor


## ------------------------------------------------------------
## by J.Kruppa on Monday, May 10, 2021 (14:49)
## Variablen Selektion nach univariate/univariable signifikant

birds_tbl <- read_csv(birds_file) %>%
  clean_names %>%
  mutate(species = tolower(species)) %>%
  select(-species) %>%
  na.omit

CreateTableOne(data = birds_tbl, factorVars = c("migr", "diet", "broods",
                                                "wood", "upland", "water"),
               strata = "status")

null_fit <- glm(status ~ 1, data = birds_tbl, family = binomial)

full_fit <- glm(status ~ length + mass + range + migr + insect +
                  diet + clutch + broods + wood + upland + water +
                  release + indiv,
                data = birds_tbl, family = binomial)

## only on signifcant variables
testing_fit <- glm(status ~ release + indiv,
                   data = birds_tbl, family = binomial) %>%
  summary

## stepwise

step_fit <- step(null_fit,
                 scope = list(upper = full_fit),
                 direction = "both") 

step_fit %>% summary

full_fit <- glm(status ~ length + range + migr + insect +
                  diet + clutch + broods + wood + upland + water +
                  release,
                data = birds_tbl, family = binomial)

step_fit <- step(null_fit ,
                 scope = list(upper = full_fit),
                 direction = "both") 

step_fit %>% summary

## standard, den man machen sollte (wenn schon AIC)
MASS::stepAIC(full_fit, direction = "backward") %>% summary

## ------------------------------------------------------------
## by J.Kruppa on Monday, May 10, 2021 (16:15)

model.1 <- glm(status ~ length + mass + range + migr + insect +
                 diet + clutch + broods + wood + upland + water +
                 release + indiv,
                data = birds_tbl, family = binomial)

model.2 <- glm(status ~ length + mass + wood + upland + water +
                 release + indiv,
               data = birds_tbl, family = binomial)

anova(model.1, model.2, test = "Chisq")

lrtest(model.1, model.2)

## EPV
(nrow(birds_tbl)/7) %>% floor

## auf events
(sum(birds_tbl$status)/7) %>% floor


