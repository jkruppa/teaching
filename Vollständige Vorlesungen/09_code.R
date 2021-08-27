## ------------------------------------------------------------
## by J.Kruppa on Monday, January 11, 2021 (16:39)
library(tidyverse)
library(broom)


chi_test_statistic <- (7-12)^2/12 + (14-9)^2/9 + (22-17)^2/17 + (8-13)^2/13

## Chi Quardrat Test in R


x <- matrix(c(7, 14, 22, 8), ncol = 2, byrow = TRUE)

chisq.test(x, correct = FALSE) ## %>% tidy()

OR <- round((7*8) / (22*14), 2)

