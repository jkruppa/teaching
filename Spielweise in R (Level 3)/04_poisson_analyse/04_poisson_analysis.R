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
cutting_tbl <- read_excel("04_poisson_analysis.xlsx", 
                          sheet = 1) %>% 
  clean_names() %>% 
  mutate(block = as_factor(block),
         trt = factor(trt, levels = c("control", "nodium_3rd",
                                      "nodium_5th", "strong"))) %>% 
  rename(leaf = "leaf_8",
         flower = "flower_8",
         fruit = "fruit_8")
  
cutting_tbl %>% str()
cutting_tbl %>% glimpse()
cutting_tbl %>% pull(trt)


## Anzahl der Blüten

flowers_tbl <- cutting_tbl %>% 
  select(trt, block, flower) %>% 
  mutate(flower = round(flower))

ggplot(flowers_tbl, aes(flower)) +
  geom_histogram()

flowers_tbl %>% 
  ggplot(aes(x = trt, y = flower, fill = block)) +
  theme_minimal() +
  geom_boxplot() 

stat_tbl <- flowers_tbl %>% 
  group_by(trt, block) %>% 
  summarise(mean = mean(flower),
            sd = sd(flower))

ggplot(stat_tbl, aes(x = trt, y = mean, group = block, fill = block)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2, position = position_dodge(0.9)) +
  theme_bw() +
  labs(fill = "Block", y = "Mittlere Anzahl an Blüten",
       x = "") +
  scale_fill_okabeito()



flowers_fit <- glm(flower ~ trt + block + trt:block, 
                   data = flowers_tbl, 
                   family = poisson())

flowers_fit %>% check_overdispersion()

flowers_fit <- glm(flower ~ trt + block + trt:block, 
                   data = flowers_tbl, 
                   family = quasipoisson())

flowers_fit %>% summary()

r2_efron(flowers_fit)

check_model(flowers_fit, 
            check = c("qq", "outliers", "pp_check", "homogeneity")) 

flowers_fit %>% 
  car::Anova() %>% 
  model_parameters()

flowers_emm_obj <- flowers_fit %>% 
  emmeans(specs = ~ trt, type = "response") 

flowers_emm_obj %>% 
  contrast(method = "pairwise", adjust = "bonferroni") 

flowers_emm_obj %>%
  cld(Letters = letters, adjust = "bonferroni") 


letter_vec <- c("b", "ab", "b", "a")

ggplot(stat_tbl, aes(x = trt, y = mean, group = block, fill = block)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2, position = position_dodge(0.9)) +
  theme_bw() +
  labs(fill = "Block", y = "Mittlere Anzahl an Blüten",
       x = "") +
  scale_fill_okabeito() +
  annotate("text", x = c(1, 2, 3, 4), c(40, 23, 32, 15),
           label = letter_vec, size = 5, color = "red")





