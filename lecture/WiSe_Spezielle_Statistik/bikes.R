library(tidyverse)
library(readxl)

bikes_tbl <- read_excel("GitHub/teaching/lecture/WiSe_Spezielle_Statistik/bikes.xlsx")
bikes_tbl <- read_excel("bikes.xlsx")


ggplot(bikes_tbl, aes(tag_im_Jahr, anzahl_cum,
                      color = as_factor(kw))) +
  theme_minimal() +
  geom_point() +
 #xlim(0, 366) +
 # ylim(0, 1e6) +
  stat_smooth(method = "lm", fullrange=FALSE, se = FALSE)

fit <- lm(anzahl_cum ~ tag_im_Jahr, data = bikes_tbl)

predict(fit, tibble(tag_im_Jahr = 366))


ggplot(bikes_tbl, aes(temp, anzahl_tag)) +
  theme_minimal() +
  geom_point() +
  scale_x_continuous(breaks = 1:20)#+
  #xlim(0, 366) +
  # ylim(0, 1e6) +
  #stat_smooth(method = "lm", se = FALSE)

bike_func <- \(x) {0 + 2936 * x}

ggplot(bikes_tbl, aes(tag_im_Jahr, anzahl_cum)) +
  theme_minimal() +
  geom_point() +
  xlim(0, 366) +
  ylim(0, 1e6) +
  geom_function(fun = bike_func, color = "red") +
  geom_hline(yintercept = 1e6) +
  geom_vline(xintercept = 365)


ggplot(bikes_tbl, aes(tag_im_Jahr, anzahl_cum)) +
  theme_minimal() +
  geom_point() +
  geom_function(fun = bike_func, color = "red") 

predict(fit, tibble(tag_im_Jahr = 366))

bikes_sep_tbl <- bikes_tbl %>% 
  magrittr::extract(1:5,)

ggplot(bikes_sep_tbl, aes(tag_im_Jahr, anzahl_cum)) +
  geom_point() +
  stat_smooth(method = "lm")

fit <- lm(anzahl_cum ~ tag_im_Jahr, 
          data = bikes_sep_tbl)

predict(fit, tibble(tag_im_Jahr = 366))


