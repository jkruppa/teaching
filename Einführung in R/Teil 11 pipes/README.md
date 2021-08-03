´´´r
## ------------------------------------------------------------
## by J.Kruppa on Tuesday, August  3, 2021 (08:45)
library(tidyverse)
library(magrittr) ## hier kommt der Pipe-Operator her

## ------------------------------------------------------------
## Standard (90-ziger Jahre)
data(iris)

iris_df <- iris

## verändert die Spaltennamen
iris_df <- set_names(iris_df, c("SL", "SW", "PL", "PW", "species"))

## entfernt alle Zeilen mit (mindestens) einem NA
iris_df <- na.omit(iris_df)

## wandelt einen data.frame in einen tibble um
iris_tbl <- as_tibble(iris_df)

## ------------------------------------------------------------
## Pipe operator: %>%

iris_tbl <- iris_df %>%
  set_names(c("SL", "SW", "PL", "PW", "species")) %>%
  na.omit() %>%
  as_tibble()


## mit "Punkt" 
iris_df %>%
  set_names(., c("SL", "SW", "PL", "PW", "species"))
