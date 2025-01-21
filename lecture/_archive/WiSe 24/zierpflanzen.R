library(tidyverse)
library(readxl)
library(see)
library(emmeans)
library(multcomp)

zier_wide_tbl <- read_excel("zierpflanzen.xlsx") %>% 
  mutate(bewässerung = as_factor(bewässerung),
         block = as_factor(block),
         behandlung = as_factor(behandlung)) 

zier_tbl <- zier_wide_tbl %>% 
  pivot_longer(cols = `Messung 1`:`Messung 12`,
               names_to = c("word", "messung"),
               names_sep = " ",
               values_to = "height") %>% 
  mutate(messung = as.numeric(messung)) %>% 
  select(-word)

zier_tbl %>% 
  ggplot(aes(x = messung, y = height, 
             color = behandlung)) +
  theme_minimal() +
 # geom_point(position = position_dodge(0.5), 
  #           size = 1.5) +
  stat_summary(fun = "mean", geom = "line",
               position = position_dodge(0.5)) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_okabeito()

zier_tbl %>% 
  filter(messung == 12) %>% 
  mutate(messung = as_factor(messung)) %>% 
  ggplot(aes(x = behandlung, y = height, fill = behandlung)) + 
  theme_minimal() +
  geom_boxplot() +
  scale_fill_okabeito()

zier_tbl %>% 
  filter(behandlung == "plenta",
         messung == 3,
         height < 3)

zier_fit <- lm(height ~ behandlung,
               data = filter(zier_tbl, messung == 12))

anova(zier_fit)

zier_fit %>% 
  emmeans( ~ behandlung, vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters, adjust = "bonferroni")
