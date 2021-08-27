## ------------------------------------------------------------
## by J.Kruppa on Monday, April 26, 2021 (08:10)
pacman::p_load(tidyverse, magrittr, mvtnorm, plyr, simstudy,
               broom, mosaic, PerformanceAnalytics, FSA,
               psych, car, rcompanion, lmtest, ggpubr, janitor,
               olsrr, tableone, blorr)

## ------------------------------------------------------------
## by J.Kruppa on Monday, April 26, 2021 (11:26)
## https://rcompanion.org/rcompanion/e_05.html
longnose_file <- file.path("c:/Users/kruppajo/Desktop/20210426", "longnose.csv")

longnose_tbl <- read_csv(longnose_file) %>%
  clean_names %>%
  mutate_if(is.character, tolower) %>%
  mutate(longnose_log = log(longnose)) %>%
  select(stream, longnose, longnose_log, everything())

## longnose_num_tbl <- select(longnose_tbl,
##                            longnose, acerage, do2, maxdepth, no3, so4, temp)

longnose_num_tbl <- longnose_tbl %>% select_if(is.numeric)

corr.test(longnose_num_tbl,
          use = "pairwise",
          method = "pearson",
          adjust = "none",     # Can adjust p-values; see ?p.adjust for options
          alpha = .05)

pairs(data = longnose_num_tbl,
      ~ longnose + acerage + do2 + maxdepth + no3 + so4 + temp)

chart.Correlation(longnose_num_tbl,
                  method = "pearson",
                  histogram = TRUE,
                  pch = 16)

ggplot(longnose_tbl, aes(log(longnose))) + geom_histogram()

## ------------------------------------------------------------
## by J.Kruppa on Monday, April 26, 2021 (14:59)
## http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/

longnose_log_tbl  <- longnose_tbl %>%
  select(-longnose)

## Cmpute MDS to look at the samples
mds_tbl <- longnose_num_tbl %>%
  select(-longnose, -longnose_log) %>% 
  dist() %>%          
  cmdscale() %>%
  as_tibble() %>%
  set_names(c("dim_1", "dim_2"))

ggscatter(mds_tbl, x = "dim_1", y = "dim_2", 
          label = longnose_log_tbl$stream,
          size = 1,
          repel = TRUE)

## Multiples linlares model
model_null <- lm(longnose ~ 1,
                data = longnose_tbl)

model.matrix(longnose ~ 1, data = longnose_tbl)

model_full <- lm(longnose ~ acerage + do2 + maxdepth + no3 + so4 + temp,
                data = longnose_tbl)

model_full %>% summary

ols_plot_resid_qq(model_full)

model_log_full <- lm(longnose_log ~ acerage + do2 + maxdepth + no3 + so4 + temp,
                data = longnose_tbl)

model_log_full %>% summary

ols_plot_resid_qq(model_log_full)

ols_test_normality(model_log_full) ## machen Sie keine Vortests

ols_plot_resid_fit(model_log_full)
ols_plot_resid_fit(model_full)

## Bedeutende Punkte
influence.measures(model_full)

ols_plot_cooksd_bar(model_full) ## cooks d
ols_plot_cooksd_bar(model_log_full)

## VIF (variance inflation factor)

sigma <- matrix(0, 2, 2)
diag(sigma) <- c(1, 1)
sigma[1,2] <- 0.9
sigma[2,1] <- 0.9

x <- rmvnorm(n = 100, mean = c(1, 5), sigma = sigma) %>%
  set_colnames(c("grp_1", "grp_2")) %>%
  as_tibble
cov(x)
var(x$grp_1)
var(x$grp_2)

## https://www.statisticshowto.com/variance-inflation-factor/

ols_vif_tol(model_log_full)
ols_vif_tol(model_full)

## und zurück
model_log_full %>% tidy() %>%
  mutate(estimate = exp(estimate))

model_full %>% tidy() 

## ------------------------------------------------------------
## by J.Kruppa on Monday, April 26, 2021 (08:10)
## https://rcompanion.org/rcompanion/e_07.html
birds_file <- file.path("c:/Users/kruppajo/Desktop/20210426", "birds.csv")

birds_tbl <- read_csv(birds_file) %>%
  clean_names %>%
  mutate_if(is.character, tolower)

birds_num_tbl <- birds_tbl %>%
  select_if(is.numeric)

options(max.print = 10000)

## wieviele fehlende Daten haben wir
CreateTableOne(data = birds_num_tbl) %>% summary

##
chart.Correlation(birds_num_tbl,
                  method = "spearman",
                  histogram = TRUE,
                  pch = 16)

## Cmpute MDS to look at the samples
mds_tbl <- birds_num_tbl %>%
  select(-status) %>% 
  dist() %>%          
  cmdscale() %>%
  as_tibble() %>%
  set_names(c("dim_1", "dim_2"))

ggscatter(mds_tbl, x = "dim_1", y = "dim_2", 
          label = birds_tbl$species,
          size = 1,
          repel = TRUE) 

## Cmpute MDS to look at the samples

birds_clean_tbl <- birds_tbl %>%
  select(-status) %>%
  filter(!species %in% c("cyg_olor")) 

mds_clean_tbl <- birds_clean_tbl %>%
  select(-species) %>% 
  dist() %>%          
  cmdscale() %>%
  as_tibble() %>%
  set_names(c("dim_1", "dim_2")) %>%
  mutate(id = birds_clean_tbl$species)

ggscatter(mds_clean_tbl, x = "dim_1", y = "dim_2", 
          label = birds_clean_tbl$species,
          size = 1,
          repel = TRUE) 

wanted_ids_clean <- mds_clean_tbl %>%
  filter(dim_1 > -1000) %>%
  pull(id)

## und erneut auf den cleanen

birds_clean_tbl <- birds_tbl %>%
  select(-status) %>%
  filter(species %in% wanted_ids_clean) 

mds_clean_tbl <- birds_clean_tbl %>%
  select(-species) %>% 
  dist() %>%          
  cmdscale() %>%
  as_tibble() %>%
  set_names(c("dim_1", "dim_2")) %>%
  mutate(id = birds_clean_tbl$species)

ggscatter(mds_clean_tbl, x = "dim_1", y = "dim_2", 
          label = birds_clean_tbl$species,
          size = 1,
          repel = TRUE) 

## unser sauberer:

birds_clean_tbl <- birds_tbl %>%
  filter(species %in% wanted_ids_clean) %>%
  na.omit

## chechen, ob ich noch 0 und 1 habe
table(birds_clean_tbl$status)


## ------------------------------------------------------------
## by J.Kruppa on Monday, April 26, 2021 (16:29)
## multiple logistsiche regression

model_null <- glm(status ~ 1,
                 data = birds_clean_tbl,
                 family = binomial(link="logit"))

model_full <- glm(status ~ length + mass + range + migr + insect + diet +
                   clutch + broods + wood + upland + water +
                   release + indiv,
                 data = birds_clean_tbl,
                 family = binomial(link="logit"))

model_null %>% summary
model_full %>% summary
model_full %>% tidy(exponentiate = TRUE)

blr_model_fit_stats(model_full)

## https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression

blr_confusion_matrix(model_full)

car::vif(model_full) %>% ldply %>%
  set_names(c("var_id", "VIF"))

chart.Correlation(birds_clean_tbl %>% select(-species),
                  method = "spearman",
                  histogram = TRUE,
                  pch = 16)

## raus mit mass and release

birds_clean_reduced_tbl <- birds_clean_tbl %>%
  select(-mass, -release, -clutch)

model_reduced <- glm(status ~ length + range + migr + insect + diet +
                       broods + wood + upland + water + indiv,
                     data = birds_clean_reduced_tbl,
                     family = binomial(link="logit"))

model_reduced %>% tidy(exponentiate = TRUE)

car::vif(model_reduced)

blr_confusion_matrix(model_reduced)

plot(fitted(model_reduced),
     rstandard(model_reduced))

model_reduced %>% summary


model_reduced %>% tidy(exponentiate = TRUE)
model_full %>% tidy(exponentiate = TRUE)

## eigenes Modell nach Korrelationsmatrix

model_my_reduced <- glm(status ~ wood + poly(indiv, 2), ## I(indiv^2)
                        data = birds_clean_reduced_tbl,
                        family = binomial(link="logit"))

model_my_reduced %>% tidy(exponentiate = TRUE)

## influence.measures(model_full)


## Hmisc

d <- Hmisc::describe(birds_tbl)

p <- plot(d)
p$Continuous

gandalf <- 1 
## You shall not pass







## ideen nächste Woche


sigma <- matrix(0, 2, 2)
diag(sigma) <- c(1, 1)

x <- rmvnorm(n = 10000, mean = c(1, 5), sigma = sigma) %>%
  set_colnames(c("grp_1", "grp_2")) %>%
  as_tibble

data_tbl <- x %>%
  gather %>%
  mutate(value_bin = case_when(value < 1.5 ~ 0,
                               value >= 1.5 ~ 1))

glm(value_bin ~ key, data = data_tbl, family = "binomial")


## %>%
## tidy(exponentiate = TRUE)



def <- defData(varname = "age", dist = "normal", formula = 10, 
               variance = 2)
def <- defData(def, varname = "female", dist = "binary", 
               formula = "2 + age * -0.2", link = "logit")
dd <- genData(100000, def)
dd
table(dd$female)

ggplot(dd, aes(age, female)) + geom_point()

glm(female ~ age, dd, family = binomial) %>%
  tidy(exponentiate = TRUE)


## ------------------------------------------------------------
## by J.Kruppa on Monday, April 19, 2021 (12:11)
## flowchart

pacman::p_load(Gmisc, glue, htmlTable, grid)
library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)
