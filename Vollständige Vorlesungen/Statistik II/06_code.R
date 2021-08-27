## ------------------------------------------------------------
## by J.Kruppa on Monday, May 17, 2021 (08:22)
pacman::p_load(pscl, haven, MASS, nnet,
               janitor, tidyverse, broom, ordinal)

## ------------------------------------------------------------
## by J.Kruppa on Monday, May 17, 2021 (08:53)
## Ordinal logistic regression
## https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

ologit_tbl <- read_dta("https://stats.idre.ucla.edu/stat/data/ologit.dta") %>%
  mutate(apply = as.factor(apply))

ologit_fit <- polr(apply ~ pared + public + gpa, data = ologit_tbl)

ologit_fit %>% summary

ologit_fit %>% confint %>% exp

coef_df <- summary(ologit_fit) %>% coef

p_n <- pnorm(abs(coef_df[, "t value"]), lower.tail = FALSE) * 2

p_t <- pt(abs(coef_df[, "t value"]), df = 3, lower.tail = FALSE) * 2


cbind(coef_df,
      p_n = round(p_n, 3),
      p_t = round(p_t, 3))

ologit_fit %>% tidy(conf.int = TRUE, exponentiate = TRUE)

## ------------------------------------------------------------
## by J.Kruppa on Monday, May 17, 2021 (15:15)
## https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

multinom_tbl <- read_dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta") %>%
  select(prog, ses, write) %>%
  mutate(prog = factor(prog, labels = c("general", "academic", "vocation")))

multinom_tbl$prog2 <- relevel(multinom_tbl$prog, ref = "academic")

multinom_fit <- multinom(prog2 ~ ses + write, data = multinom_tbl)

multinom_fit %>% summary

multinom_fit %>% confint %>%  exp

z_mat <- summary(multinom_fit)$coefficients/summary(multinom_fit)$standard.errors

p_n <- (1 - pnorm(abs(z_mat), 0, 1)) * 2

multinom_fit %>% tidy(conf.int = TRUE, exponentiate = TRUE)


## ------------------------------------------------------------
## by J.Kruppa on Monday, May 17, 2021 (15:56)
## separate logistische Regression

ologit_tbl <- read_dta("https://stats.idre.ucla.edu/stat/data/ologit.dta") %>%
  mutate(apply = as.factor(apply))

ologit_tbl$apply %>% table

ologit_01_tbl <- ologit_tbl %>%
  filter(apply %in% c(0, 1))

ologit_02_tbl <- ologit_tbl %>%
  filter(apply %in% c(0, 2))

ologit_12_tbl <- ologit_tbl %>%
  filter(apply %in% c(1, 2))

## dreimal regression

log_fit_01 <- glm(apply ~ pared + public + gpa, data = ologit_01_tbl,
                  family = binomial)

log_fit_02 <- glm(apply ~ pared + public + gpa, data = ologit_02_tbl,
                  family = binomial)

log_fit_12 <- glm(apply ~ pared + public + gpa, data = ologit_12_tbl,
                  family = binomial)


log_fit_01 %>% tidy(conf.int = TRUE, exponentiate = TRUE)
log_fit_02 %>% tidy(conf.int = TRUE, exponentiate = TRUE)
log_fit_12 %>% tidy(conf.int = TRUE, exponentiate = TRUE)


## ------------------------------------------------------------
## by J.Kruppa on Monday, May 17, 2021 (16:15)
## Poisson regression

longnose_tbl <- read_csv("longnose.csv") %>%
  clean_names %>%
  select(-stream)

poisson_fit <- glm(longnose ~ acerage + do2 + maxdepth + no3 + so4 + temp,
                   longnose_tbl, family = poisson)

normal_fit <- glm(longnose ~ acerage + do2 + maxdepth + no3 + so4 + temp,
                  longnose_tbl, family = gaussian)

poisson_fit %>% summary

## Überschlagsrechnung für die Dispersion
## (aus dem glm summary): Residual deviance: 1590.04  on 61  degrees of freedom
1590.04/61

quasipoisson_fit <- glm(longnose ~ acerage + do2 + maxdepth + no3 + so4 + temp,
                        longnose_tbl, family = quasipoisson)

quasipoisson_fit %>% summary

negativebinomial_fit <- glm.nb(longnose ~ acerage + do2 + maxdepth + no3 + so4 + temp,
                           longnose_tbl)

negativebinomial_fit %>% summary


## wir machen mit den quasifit weiter

quasipoisson_fit %>% tidy(conf.int = TRUE, exponentiate = TRUE)

## How much is the fish?
sample_size <- 10000
longnose_small_tbl <- tibble(grp = rep(c(0, 1), each = sample_size),
         resp = 15 + 10 * grp + rnorm(2 * sample_size, 0, 1)) %>%
  mutate(resp_count = round(resp),
         grp = factor(grp, labels = c("ctrl", "trt")))

ggplot(longnose_small_tbl, aes(x = grp, y = resp, fill = grp)) +
  geom_boxplot()

glm(resp_count ~ grp, data = longnose_small_tbl, family = gaussian)

glm(resp_count ~ grp, data = longnose_small_tbl, family = poisson) %>%
  tidy(exponentiate = TRUE)

quasipoisson_fit <- glm(longnose ~ temp,
                        longnose_tbl, family = quasipoisson)
quasipoisson_fit %>% tidy(conf.int = TRUE, exponentiate = TRUE)


gaussian_fit <- glm(longnose ~ temp,
                        longnose_tbl, family = gaussian)
gaussian_fit %>% tidy(conf.int = TRUE, exponentiate = TRUE)

hist(longnose_tbl$longnose)

summary(longnose_tbl$longnose)

## zero inflation
zinb_tbl <- read_csv("https://stats.idre.ucla.edu/stat/data/fish.csv")


summary(zinb_tbl$count)

zeroinf_fit <- zeroinfl(count ~ child + camper | persons, data = zinb_tbl,
                        dist = "negbin")

zeroinf_fit %>% summary

## logistische regression

birds_tbl <- read_csv("birds.csv") %>%
  clean_names 

quasipoisson_fit <- glm(status ~ length + wood,
                        birds_tbl, family = quasipoisson)
quasipoisson_fit %>% tidy(conf.int = TRUE, exponentiate = TRUE)



## 
gandalf <- 1 + 1






















sample_size <- 100
ordinal_small_tbl <- tibble(grp = rep(c(0, 1, 2), each = sample_size),
         resp = 10 + 2 * grp + rnorm(3 * sample_size, 0, 1)) %>%
  mutate(resp_fac = factor(case_when(resp <= 11 ~ 1,
                                     resp > 11 & resp <= 13 ~ 2,
                                     resp > 13 ~ 3)),
         grp = factor(grp, labels = c("ctrl", "dose1", "dose2")))

ggplot(ordinal_small_tbl, aes(x = grp, y = resp, fill = grp)) +
  geom_boxplot()

ord_fit <- polr(resp_fac ~ grp, data = ordinal_small_tbl, Hess=TRUE)

ord_fit %>% tidy(exponentiate = FALSE)

clmFit <- clm(resp_fac ~ grp, data = ordinal_small_tbl)
summary(clmFit)


sample_size <- 100000
foo <- tibble(x = c(rnorm(sample_size, 1, 1),
                    rnorm(sample_size, 1.5, 1),
                    rnorm(sample_size, 2, 1)),
              y = factor(rep(c(1, 2, 3), each = sample_size)))
       
## ggplot(foo, aes(x, y)) + geom_point()

ord_fit <- polr(y ~ x, data = foo)

ord_fit %>% summary
ord_fit %>% tidy(exponentiate = TRUE)

## ------------------------------------------------------------
## by J.Kruppa on Monday, May 17, 2021 (08:53)
## https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

ml <- read_dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")


## ------------------------------------------------------------
## by J.Kruppa on Monday, May 17, 2021 (08:56)

longnose_tbl <- read_csv("longnose.csv") %>%
  clean_names %>%
  select(-stream)
  
glm(longnose ~ ., data = longnose_tbl, family = poisson) %>% tidy(exponentiate = TRUE)

glm(longnose ~ ., data = longnose_tbl, family = poisson) %>% summary

1590.04/61

glm(longnose ~ ., data = longnose_tbl, family = quasipoisson) %>% summary


## ------------------------------------------------------------
## by J.Kruppa on Monday, May 17, 2021 (10:49)
## Was sagt den jetzt der Effektschätzer?

sample_size <- 10000 
longnose_small_tbl <- tibble(grp = rep(c(0, 1), each = sample_size),
                             resp = 10 + 5 * grp + rnorm(2 * sample_size, 0, 1)) %>%
  mutate(resp_count = round(resp),
         grp = factor(grp, labels = c("ctrl", "trt")))

ggplot(longnose_small_tbl, aes(x = grp, y = resp, fill = grp)) +
  geom_boxplot()

glm(resp_count ~ grp, data = longnose_small_tbl)

glm(resp_count ~ grp, data = longnose_small_tbl, family = poisson) %>%
  tidy(exponentiate = TRUE)
