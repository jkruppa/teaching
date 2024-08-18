## ------------------------------------------------------------
## by J.Kruppa on Monday, November  9, 2020 (16:42)
library(tidyverse)

## ------------------------------------------------------------
## by J.Kruppa on Monday, November  9, 2020 (16:43)
## Datensatz einlesen
wide_format_tbl <- read_csv("wide_format.csv")

wide_format_tbl %>%
  ## select(-subject, -sex) %>%
  gather(hasenmuckel, measur, ctrl:cond3) %>%
  pull(hasenmuckel) %>% 
  tabyl

## http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
## R braucht das "long format"

library(janitor)

test_df <- as.data.frame(matrix(ncol = 6))
names(test_df) <- c("firstName", "ábc@!*", "% successful (2009)",
                    "REPEAT VALUE", "REPEAT VALUE", "")

test_df %>% clean_names

## ------------------------------------------------------------
## by J.Kruppa on Monday, November  9, 2020 (17:04)
## 

jump_long_tbl <- read_csv("jump_length.csv")

vec <- c(3, 4, 5, 8, 10)

vec_mean <- mean(vec)

(vec - vec_mean) %>% abs

mean(vec)
var(vec)
sd(vec) %>% round(2)

pdf("jump.pdf", width = 5, height = 5)
ggplot(jump_long_tbl, aes(x = type, y = jump_length, fill = type)) +
  geom_boxplot()
dev.off()

median()
quantile(jump_long_tbl$jump_length)

jump_long_tbl$jump_length %>% summary

