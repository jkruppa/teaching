## ------------------------------------------------------------
## by J.Kruppa on Monday, November 23, 2020 (10:43)
library(tidyverse)
library(plyr)

## ------------------------------------------------------------
## by J.Kruppa on Monday, November 23, 2020 (16:31)
## Übungsdatensatz
data_tbl <- read_delim("jump_length_raw.txt", delim = " ") %>%
  mutate(jump_length = as.numeric(jump_length)) 

str_which(data_tbl$jump_length, pattern = "-$")

## str_replace um das "-" los zu werden 
                                        
## stringr

## ------------------------------------------------------------
## by J.Kruppa on Monday, November 23, 2020 (16:41)
jump_tbl <- read_csv("jump_length.csv") %>%
  mutate(jump_length_cut = cut(jump_length,
                               breaks = c(-Inf, 10, 20, 30, 40, Inf)))

cut(jump_tbl$jump_length, breaks = c(-Inf, 10, 20, 30, 40, Inf))

## Wir wollen ein Histogramm für jump_length

pdf("example_plot.pdf", width = 8, height = 8)
ggplot(jump_tbl, aes(x = jump_length)) + geom_histogram()
dev.off()

## Wir wollen ein Histogramm für jump_length

jump_length_cut_tbl <- table(jump_tbl$jump_length_cut) %>%
  ldply() %>%
  as_tibble %>%
  set_names(c("jump_length_cut", "jump_length_count")) %>%
  mutate(jump_length_freq = jump_length_count/sum(jump_length_count),
         jump_length_cut = as.factor(jump_length_cut))

pdf("example_plot.pdf", width = 8, height = 8)
ggplot(jump_length_cut_tbl, aes(x = jump_length_cut,
                                y = jump_length_freq)) +
  geom_bar(stat="identity")
dev.off()


## ------------------------------------------------------------
## by J.Kruppa on Monday, November 23, 2020 (17:11)
## Zentraler Grenzwertsatz

pdf("example_plot.pdf", width = 8, height = 8)
tibble(v1 = rnorm(100, 5, 3),
       v2 = rnorm(100, 2, 3),
       v3 = rnorm(100, 6, 3),
       v4 = rnorm(100, 8, 1)) %>%
  gather %>%
  mutate(key = as.factor(key)) %>% 
  ggplot(aes(x = value, fill = key)) +
  geom_histogram() ## position = "dodge"
dev.off()

## Können wir auch subtrahieren?

pdf("example_plot.pdf", width = 8, height = 8)
tibble(d_1 = rnorm(100000, 5, 1),
       d_2 = rnorm(100000, 0, 10),
       d_diff = d_2 - d_1) %>%
  ggplot(aes(x = d_diff)) + geom_density()
dev.off()

## ------------------------------------------------------------
## by J.Kruppa on Monday, November 23, 2020 (17:24)

pdf("example_plot.pdf", width = 8, height = 8)
tibble(x = rnorm(10000, mean = 8, sd = 2),
       x_diff = x - mean(x),
       x_standard = x_diff/sd(x)) %>%
  gather %>% 
  ggplot(aes(x = value, color = key)) + geom_density()
dev.off()





stopper

























tibble(v1 = rnorm(100, 5, 3),
       v2 = rnorm(100, 2, 3),
       v3 = rnorm(100, 6, 3)) %>%
  gather %>%
  ggplot(aes(x = value, fill = key)) +
  geom_histogram(position = "dodge") 


data.frame(x = c(5, 1, 3, 2, 2, 3)) %>%
  mutate(bin_w = cut_width(x, width = 2, center = 1),
         bin_i = cut_interval(x, n = 2),
         bin_c = cut(x, c(-Inf, 1, 4, Inf)))





foo <- tibble(x = rnorm(100, 10, 5),
              mean_x = mean(x),
              x_shift = x - mean_x,
              x_stand = x_shift/sd(x))

foo

ggplot(foo, aes(x= x_stand)) + geom_density()
