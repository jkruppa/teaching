library(tidyverse)
library(broom)

n_group <- 100
data_tbl <- tibble(jump_length = c(rnorm(n_group, 30, 9.1),
                                   rnorm(n_group, 19.9, 9.1)),
                   group = rep(c("dog", "cat"), each = n_group)) %>%
  mutate(group = as.factor(group))

stat_tbl <- t.test(jump_length ~ group, data = data_tbl, var.equal = TRUE) %>%
  tidy

str_c(round(stat_tbl$estimate, 2), " [",
      round(stat_tbl$conf.low, 2), "; ",
      round(stat_tbl$conf.high, 2), "]")

foo <- t.test(jump_length ~ group, data = data_tbl, var.equal = TRUE)

foo$conf.int
