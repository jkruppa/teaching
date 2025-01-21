library(tidyverse)
library(readxl)
library(janitor)
library(emmeans)
library(multcomp)

soil_tbl <- read_excel("C:/Users/jokruppa/Desktop/2024_Master.xlsx")

soil_tbl <- soil_tbl %>% 
  clean_names() %>% 
  mutate(variante = as_factor(variante),
         tiefe = as_factor(tiefe),
         block = as_factor(block)) %>% 
  select(-proben_id)

## ag_st

soil_tbl %>% 
  ggplot(aes(x = variante, y = ag_st, fill = tiefe)) +
  geom_boxplot()

soil_tbl %>% 
  ggplot(aes(x = variante, y = ag_st, shape = tiefe,
             color = block)) +
  geom_point(position = position_dodge(0.9), size = 5)

soil_tbl %>% 
  ggplot(aes(x = variante, y = ag_st, fill = tiefe)) +
  geom_boxplot()

soil_tbl %>% 
  ggplot(aes(x = tiefe, y = ag_st, fill = variante)) +
  geom_boxplot()

soil_tbl %>% 
  ggplot(aes(x = tiefe, y = ag_st, shape = variante,
             color = block)) +
  geom_point(position = position_dodge(0.9), size = 5)


ag_st_fit <- lm(ag_st ~ variante + tiefe + variante:tiefe,
                data = soil_tbl)

ag_st_fit %>% anova()

ag_st_fit %>% 
  emmeans(~ variante * tiefe,
          vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters, 
      adjust = "none")



## cmik

soil_tbl %>% 
  ggplot(aes(x = variante, y = cmik, shape = tiefe,
             color = block)) +
  geom_point(position = position_dodge(0.9), size = 5)
