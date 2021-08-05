```r

## ------------------------------------------------------------
## by J.Kruppa on Friday, July 30, 2021 (08:42)
library(tidyverse)
library(magrittr)

## Read in csv 

data_tbl <- read_csv("dummy_data.csv")

## dput

names(data_tbl) %>% dput

names_full_vec <- c("age", "gender", "activity", "frailty", "surgery", "bloodpressure", 
                    "height", "weight", "creatinin", "ISCED", "pet", "CRP", "ASA", 
                    "clinic", "complication", "POD", "without_complication", "anae_start", 
                    "anae_end")

## select

data_tbl %>%
  select(pet, age, sex = gender, pod = POD, everything())


names_select_vec <- c("age", "gender", "activity", "frailty", "surgery", "bloodpressure", 
                      "height")

data_tbl %>%
  select(all_of(names_select_vec))

## filter

data_clean_tbl <- data_tbl %>%
  select(pet, age, sex = gender, pod = POD) %>%
  filter(pet == "no")

data_clean_tbl %>%
  pull(pet) %>%
  table

data_clean_tbl <- data_tbl %>%
  select(pet, age, sex = gender, pod = POD) %>%
  filter(pet == "no" | sex == "woman")

data_clean_tbl <- data_tbl %>%
  select(pet, age, sex = gender, pod = POD, frailty) %>%
  filter(pet == "no" & sex == "woman") %>%
  filter(frailty %in% c("frail", "pre-frail")) %>%
  filter(age > 60)

## &: UND Operator (logisch)
## |: ODER Operator (logisch)
## %in% IN Operator (logisch)

## logische Sachen

data_clean_tbl$age >= 65

data_tbl$frailty %in% c("frail", "pre-frail")

## mutate

data_clean_tbl <- data_tbl %>%
  select(age, sex = gender, frailty, anae_start, anae_end,
         weight, height) %>%
  filter(age > 60)

## transformieren: (anae_end, anae_start) oder BMI

data_clean_tbl %>%
  mutate(bmi = height/weight^2,
         duration = anae_end - anae_start,
         age_plus_10 = age + 10) 

## case_when

data_clean_tbl %>%
  mutate(age_bin = case_when(age >= 64 ~ "old",
                             age < 64 & age >= 62 ~ "medium",
                             age < 62 ~ "young")) %>%
  select(age, age_bin)

## factor

data_clean_tbl %>%
  select(frailty, sex) %>%
  mutate(sex = as.factor(sex),
         frailty = factor(frailty, labels = c("frail", "pre-frail", "robust"))) %>%
  pull(frailty)
```
