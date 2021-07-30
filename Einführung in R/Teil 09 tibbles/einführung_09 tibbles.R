## ------------------------------------------------------------
## by J.Kruppa on Friday, July 30, 2021 (08:05)
library(tidyverse)

## tibble
data(starwars)

starwars_tbl <- starwars 

ncol(starwars_tbl)
nrow(starwars_tbl)
dim(starwars_tbl)
glimpse(starwars_tbl)

## data.frame
data(iris)

iris_df <- iris

row.names(iris_df) <- str_c("plant_id_", 1:nrow(iris_df))

iris_tbl <- as_tibble(iris_df, rownames = "plant_id")
