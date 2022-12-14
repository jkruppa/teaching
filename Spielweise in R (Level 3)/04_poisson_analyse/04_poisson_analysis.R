## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
## install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor,
               emmeans, multcomp, magrittr,
               parameters, effectsize,
               multcompView, see, performance,
               conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("extract", "magrittr")

## wir lesen immer nur ein Tabellenblatt ein
cutting_tbl <- read_excel("04_poisson_analysis.xlsx", 
                              sheet = 1) %>% 
  clean_names() %>% 
  mutate(block = as_factor(block),
         trt = as_factor(trt)) %>% 
  rename(leaf = "leaf_8",
         flower = "flower_8",
         fruit = "fruit_8")
  
## Anzahl der Blüten

flowers_tbl <- cutting_tbl %>% 
  select(trt, block, flower)

flowers_tbl %>% 
  ggplot(aes(x = trt, y = flower, fill = block)) +
  theme_minimal() +
  geom_boxplot() 


flowers_tbl <- cutting_tbl %>% 
  select(trt, block, flower)

flowers_fit <- glm(flower ~ trt + block, 
                   data = flowers_tbl, 
                   family = poisson())

flowers_fit %>% check_overdispersion()

flowers_fit <- glm(flower ~ trt + block + trt:block, 
                   data = flowers_tbl, 
                   family = quasipoisson())

flowers_fit %>% 
  car::Anova() %>% 
  model_parameters()

flowers_emm_obj <- flowers_fit %>% 
  emmeans(specs = ~ trt, type = "response") 

flowers_emm_obj %>% 
  contrast(method = "pairwise", adjust = "bonferroni") 

flowers_emm_obj %>%
  cld(Letters = letters, adjust = "bonferroni") 







