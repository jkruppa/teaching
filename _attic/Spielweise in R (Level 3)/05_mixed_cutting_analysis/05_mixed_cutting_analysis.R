## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
## install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,
               emmeans, multcomp, magrittr,
               parameters, effectsize,
               multcompView, see, 
               lme4, performance,
               conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("extract", "magrittr")

## wir lesen immer nur ein Tabellenblatt ein
cutting_raw_tbl <- read_excel("05_mixed_cutting_analysis.xlsx", 
                              sheet = 1) %>% 
  clean_names() 


cutting_tbl <- cutting_raw_tbl %>% 
  select(trt, block, matches("shoot")) %>% 
  pivot_longer(cols = shoot_1:last_col(), 
               names_to = c("outcome", "week"), 
               names_sep = "_",
               values_to = "rsp") %>% 
  arrange(outcome, week, trt, block, rsp) %>% 
  mutate(block = as_factor(block),
         trt = as_factor(trt),
         outcome = as_factor(outcome),
         week = as.numeric(week))


cutting_tbl %>% 
  ggplot(aes(x = week, y = rsp, color = trt, linetype = block)) +
  theme_minimal() +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  scale_color_okabeito()

## linearen gemischten Modell
lmer_fit <- lmer(rsp ~ trt + block + trt:block + (1|week), 
                 data = cutting_tbl)

lmer_fit %>% r2

## Das Conditional R2 ist der erklärte Anteil der Varianz von 
## den festen und zufälligen Effekten zusammen. 
## Das Marginal R2 ist der erklärte Anteil der Varianz von 
## den festen Effekten alleine. 

lmer_fit %>% summary()

## Wir können dann noch den Intraclass Correlation Coefficient 
## (abk. ICC) berechnen. Der ICC beschreibt den Anteil 
## der Varianz, der durch die Gruppierungsstruktur 
## in der Stichprobe erklärt wird. 

109.134 / (109.134 + 13.818)

lmer_fit %>% icc

lmer_fit %>% car::Anova()

emm_obj <- lmer_fit %>% 
  emmeans(specs = ~ trt) 

emm_obj %>% 
  contrast(method = "pairwise", adjust = "bonferroni") 

emm_obj %>%
  cld(Letters = letters, adjust = "bonferroni") 

emm_obj %>% 
  contrast(method = "trt.vs.ctrlk", ref = 1, 
           adjust = "bonferroni", reverse = TRUE)
