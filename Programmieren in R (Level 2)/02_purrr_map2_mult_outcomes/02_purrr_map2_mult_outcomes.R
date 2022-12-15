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
conflict_prefer("summarise", "dplyr")
conflict_prefer("extract", "magrittr")

## wir lesen immer nur ein Tabellenblatt ein
cutting_tbl <- read_excel("02_purrr_map2_mult_outcomes.xlsx", 
                          sheet = 1) %>% 
  clean_names() %>% 
  mutate(block = as_factor(block),
         trt = as_factor(trt)) %>% 
  pivot_longer(cols = leaf:last_col(),
               names_to = "outcome",
               values_to = "rsp") %>% 
  arrange(outcome, trt, block)

  
cutting_lst <- cutting_tbl %>% 
  split(.$outcome)

family_lst <- lst(ca = gaussian(), 
                  drymatter = gaussian(), 
                  flower = quasipoisson(),
                  freshweight = gaussian(), 
                  fruit = quasipoisson(),
                  height = gaussian(),
                  leaf = quasipoisson())

glm_lst <- cutting_lst %>% 
  map2(family_lst, ~glm(rsp ~ trt + block + trt:block, 
           data = .x, family = .y))  
  
glm_lst %>% 
  map(pluck, "family")

glm_lst %>% 
  map(car::Anova)

emm_lst <- glm_lst %>% 
  map(~emmeans(.x, specs = ~ trt, type = "response")) 

emm_lst %>% 
  map(~contrast(.x, method = "pairwise", adjust = "bonferroni")) %>% 
  map(as_tibble) %>% 
  bind_rows(.id = "outcome")

emm_lst %>% 
  map(~cld(.x, Letters = letters, adjust = "bonferroni")) %>% 
  map(as_tibble) %>% 
  bind_rows(.id = "outcome") %>% 
  select(outcome, trt, .group) # %>% 
##   print(n = 28)

## write_csv2()



