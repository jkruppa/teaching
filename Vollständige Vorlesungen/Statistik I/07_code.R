## ------------------------------------------------------------
## by J.Kruppa on Monday, December 14, 2020 (16:51)
library(tidyverse)
library(broom)

## ------------------------------------------------------------
## by J.Kruppa on Monday, December 14, 2020 (16:51)

data_tbl <- tibble(sprungweite = c(rnorm(100, 12.8, 5),
                                   rnorm(100, 15, 5)),
                   type = rep(c("dog", "cat"), each = 100)) %>%
  mutate(type = as.factor(type))

pdf("plot.pdf", width = 8, height = 8)
ggplot(data_tbl, aes(type, sprungweite)) + geom_boxplot()
dev.off()

## h_0:  x1 == x2
## h_A:  x1 != x2
## p < 0.05 -> H_0 ablehnen

t.test(sprungweite ~ type, data = data_tbl) ## %>% tidy()
