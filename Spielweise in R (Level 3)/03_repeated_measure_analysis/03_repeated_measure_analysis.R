## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
## install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,
               emmeans, multcomp, magrittr,
               parameters, effectsize,
               multcompView, see, psych,
               conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("extract", "magrittr")

## wir lesen immer nur ein Tabellenblatt ein
cutting_raw_tbl <- read_excel("03_repeated_measure_analysis.xlsx") %>% 
  clean_names() 



cutting_tbl <- cutting_raw_tbl %>% 
  pivot_longer(cols = shoot_1:fruit_8, 
               names_to = c("outcome", "week"), 
               names_sep = "_",
               values_to = "rsp") %>% 
  arrange(outcome, week, trt, block, rsp) %>% 
  mutate(block = as_factor(block),
         trt = as_factor(trt),
         outcome = as_factor(outcome),
         week = as.numeric(week))



## Abbildung machen
cutting_plot_tbl <- cutting_tbl %>% 
  mutate(outcome = recode(outcome, 
                          shoot = "Trieblänge", 
                          flower = "Blütenanzahl", 
                          leaf = "Blätteranzahl", 
                          fruit = "Fruchtanzahl"))

cutting_plot_tbl %>% 
  ggplot(aes(week, rsp, color = trt, linetype = block)) +
  theme_minimal() +
  facet_wrap(~ outcome, scales = "free_y") +
  geom_point() +
  stat_smooth(se = FALSE) +
  scale_color_okabeito()



cutting_cor_tbl <- cutting_tbl %>% 
  filter(week %in% c(6, 7, 8)) %>% 
  pivot_wider(names_from = outcome, values_from = rsp) %>% 
  unnest(cols = c(flower, fruit, leaf, shoot))

cutting_cor_tbl %>% 
  select(flower:shoot) %>% 
  pairs.panels(smooth = TRUE, density = TRUE, method = "kendall", lm = FALSE, 
               cor = TRUE, ellipses = FALSE, stars = TRUE)    




shoot_length_tbl <- cutting_tbl %>% 
  filter(outcome == "shoot" & week == 8)

shoot_length_fit <- lm(rsp ~ trt + block, data = shoot_length_tbl)

shoot_length_fit %>% 
  anova() %>% 
  model_parameters()

shoot_length_fit %>% eta_squared()

shoot_emm_obj <- shoot_length_fit %>% 
  emmeans(specs = ~ trt) 

shoot_emm_obj %>% 
  contrast(method = "pairwise", adjust = "bonferroni")

shoot_emm_obj %>%
  cld(Letters = letters, adjust = "bonferroni") 

