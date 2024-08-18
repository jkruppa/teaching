## ------------------------------------------------------------
## by J.Kruppa on Monday, May 31, 2021 (08:35)
pacman::p_load(tidyverse, lubridate,
               survminer, RTCGA.clinical, survival,
               naniar, cutpointr, Hmisc, janitor,
               magrittr, broom)

## ------------------------------------------------------------
## by J.Kruppa on Monday, May 31, 2021 (14:43)
## load data set
data(lung)

## anschauen wie so die missings sind
vis_miss(lung)

## sauberen datensatz
lung_tbl <- lung %>%
  clean_names %>% 
  as_tibble %>%
  mutate(meal_cal = Hmisc::impute(meal_cal, median),
         wt_loss = Hmisc::impute(wt_loss, median),
         event = status - 1) %>%
  select(inst, time, status, event, everything()) %>% 
  na.omit

hist(lung_tbl$time)

ggplot(lung_tbl, aes(time, fill = as.factor(status))) + geom_histogram()

lung_tbl$time %>% summary

lung_tbl %>%
  select(time, status) %>% 
  add_column(as.character(Surv(time = lung_tbl$time,
                               event = lung_tbl$status,
                               type = "right")))

f1 <- survfit(Surv(time = lung_tbl$time,
                   event = lung_tbl$status,
                   type = "right") ~ 1, data = lung)
names(f1)

## hässlich
survfit(Surv(time = time,
                  event = status,
             type = "right") ~ 1, data = lung) %>%
  plot(xlab = "Days", 
       ylab = "Overall survival probability")

## hübisch
ggsurvplot(
  fit = survfit(Surv(time, status) ~ 1, data = lung), 
  xlab = "Days", 
  ylab = "Overall survival probability",
  risk.table = TRUE)

## 1 Jahr überleben
summary(survfit(Surv(time, status) ~ 1, data = lung), times = 365.25)


ggsurvplot(
  fit = survfit(Surv(time, status) ~ sex, data = lung), 
  xlab = "Days", 
  ylab = "Overall survival probability",
  risk.table = TRUE)

## dichotomizieren
lung_tbl %<>%
  mutate(age_bin = ifelse(age <= median(age), 0, 1)) ## %>%
  ## filter(time < 720)

ggsurvplot(
  fit = survfit(Surv(time, status) ~ ph_ecog, data = lung_tbl), 
  xlab = "Days", 
  ylab = "Overall survival probability",
  risk.table = TRUE)

## test for sex strata
ggsurvplot(
  fit = survfit(Surv(time, status) ~ sex + age_bin, data = lung_tbl), 
  xlab = "Days", 
  ylab = "Overall survival probability",
  risk.table = TRUE,
  pval = TRUE,
  break.time.by = 90,
  xlim = c(0, 720))

survdiff(Surv(time, status) ~ sex, data = lung_tbl)

## cox ph multiple survial regression analysis
fit <- coxph(Surv(time, status) ~ sex, data = lung_tbl)
fit %>% tidy(conf.int = TRUE, exponentiate = TRUE)

fit <- coxph(Surv(time, status) ~ sex + age + ph_ecog +
           age:sex, data = lung_tbl)

fit %>% tidy(conf.int = TRUE, exponentiate = TRUE)

## 3 Monatsüberleben
lung_tbl %<>%
  mutate(mort_3_month = ifelse(time <= 90, 1, 0))

lung_tbl$mort_3_month %>% table

glm(mort_3_month ~ sex + age, family = binomial, data = lung_tbl) %>%
  tidy(conf.int = TRUE, exponentiate = TRUE)

## vgl
coxph(Surv(time, status) ~ sex + age, data = lung_tbl) %>%
  tidy(conf.int = TRUE, exponentiate = TRUE)



## ------------------------------------------------------------
## by J.Kruppa on Monday, May 31, 2021 (15:19)
date_ex <- tibble(
    sx_date = c("2007-06-22", "2004-02-13", "2010-10-27"), 
    last_fup_date = c("2017-04-15", "2018-07-04", "2016-10-31"))

date_ex %>% 
  mutate(
    sx_date = as.Date(sx_date, format = "%Y-%m-%d"), 
    last_fup_date = as.Date(last_fup_date, format = "%Y-%m-%d") 
    ) %>% 
  mutate(
    os_yrs = as.numeric(difftime(last_fup_date, 
                                 sx_date, 
                                 units = "days")) / 365.25 
    ) %>% 
  mutate(
    os_yrs_lubridate = 
      as.duration(sx_date %--% last_fup_date) / dyears(1)
    )


## %Y für 2001

cp <- cutpointr(lung, meal.cal, status, 
                method = maximize_metric, metric = sum_sens_spec,
                na.rm = TRUE)

plot(cp)


impute(lung$meal.cal, mean) %>% as.numeric()
