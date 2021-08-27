## ------------------------------------------------------------
## by J.Kruppa on Monday, December  7, 2020 (15:48)
library(tidyverse)
library(mosaic)
library(broom)

## ------------------------------------------------------------
## by J.Kruppa on Monday, December  7, 2020 (17:04)

data_tbl <- tibble(jump_length = c(rnorm(100, 30, 9),
                                   rnorm(100, 19, 9)),
                   type = rep(c("cat", "dog"), each = 100))

mosaic::mean(jump_length ~ type, data = data_tbl)
mosaic::sd(jump_length ~ type, data = data_tbl)

t_test_own <- function(x1, x2, sd1, sd2, n_group){
  s_p <- (sd1 + sd2)/2
  t <- (x2 - x1)/(s_p * sqrt(2/n_group))
  return(t)
}

t_test_own(x2 = 29.69, x1 = 18.07, sd1 = 7.96, sd2 = 8.41, n_group = 100)

pdf("plot.pdf", width = 5, height = 5)
ggplot(data_tbl, aes(x = type, y = jump_length)) + geom_boxplot() +
  coord_flip()
dev.off()


t.test(jump_length ~ type, data = data_tbl, var.equal = TRUE) ## %>% tidy()

