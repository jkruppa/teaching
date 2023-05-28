## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
pacman::p_load(tidyverse, janitor, fs,
               reshape2, broom, readxl,
               scales, multcompView, multcomp,
               rcompanion, see)


rauch_tbl <- read_excel("/Users/kruppajo/Documents/GitHub/teaching/Spielweise in R (Level 3)/10_smoking_vanille/smoking_vanille.xlsx",
                        sheet = 1) %>% 
  clean_names() 

rauch_long_tbl <- rauch_2_tbl %>% 
  pivot_longer(rauch_1:kontrolle,
               values_to = "gekeimt",
               names_to = "trt") %>% 
  mutate(trt = as_factor(trt),
         gekeimt_perc = gekeimt/30) %>% 
  filter(tag < 34)

ggplot(rauch_long_tbl, aes(tag, gekeimt, color = trt)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  labs(color = "Versuchsgruppe", 
       x = "Tag der Messung",
       y = "Anzahl der gekeimten Pflanzen")

time_tbl <- rauch_long_tbl %>% 
  filter(tag %in% c(15, 21, 31)) 

acast(time_tbl, trt ~ tag, value.var = "gekeimt") %>% 
  chisq.test(correct = FALSE)

time_tbl <- rauch_long_tbl %>% 
  filter(tag %in% c(15, 21, 31)) %>% 
  filter(trt != "kontrolle")

acast(time_tbl, trt ~ tag, value.var = "gekeimt") %>% 
  chisq.test(correct = FALSE)

prop.test(c(6, 2, 13, 8), c(30, 30, 30, 30))

acast(time_tbl, tag ~ trt, value.var = "gekeimt") %>% 
  asplit(1) %>% 
  map(~prop.test(.x, n = c(30, 30, 30, 30))) %>% 
  map_dfr(tidy, .id = "tag") %>% 
  mutate(p.value = pvalue(p.value))
  

pairwise.prop.test(c(rauch_1 = 6, 
                     rauch_2 = 2, 
                     kaffe_1 = 13, 
                     kaffe_2 = 8), n = c(30, 30, 30, 30), 
                   p.adjust.method = "none") %>% 
  pluck("p.value") %>% 
  fullPTable() %>% 
  multcompLetters() %>% 
  pluck("Letters")

ggplot(rauch_long_tbl, aes(tag, gekeimt, color = trt)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  labs(color = "Versuchsgruppe", 
       x = "Tag der Messung",
       y = "Anzahl der gekeimten Pflanzen") +
  geom_vline(xintercept = c(15, 21, 31), linetype = 2,
             alpha = 0.75) +
  annotate("text", x = c(15, 21, 31) + 0.2,
           y = 10, label = c("<0.001", "0.006", "0.010"), hjust = 0,
           alpha = 0.75) +
  annotate("text", x = c(31) + 0.3,
           y = c(6, 2, 13, 8) + 0.6, label = c("ab", "a", "b", "ab"), hjust = 0,
           size = 5) +
  scale_color_okabeito()


