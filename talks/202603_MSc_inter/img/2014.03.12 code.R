library(tidyverse)

## Zahlen in ein Objekt abspeichern

num_vec <- c(120, 145, 155)

mean(num_vec)
sd(num_vec)

library(readxl)
water_tbl <- read_excel("C:/Users/jokruppa/Desktop/zerforschen_barplot_simple.xlsx", 
                        sheet = "RData")

as_factor(water_tbl$group)

water_tbl <- water_tbl %>% 
  mutate(group_fct = as_factor(group))

stat_tbl <- water_tbl %>% 
  mutate(group = as_factor(group)) %>% 
  group_by(group) %>% 
  summarise(mean = mean(nitrat_konzentration),
            sd = sd(nitrat_konzentration),
            var = var(nitrat_konzentration))


ggplot(stat_tbl, aes(x = group, y = mean, fill = group)) + 
  theme_classic() +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean-2*sd, ymax = mean+2*sd),
                width = 0.2) +
  theme(legend.position = "none")
