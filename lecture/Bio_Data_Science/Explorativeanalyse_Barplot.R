pacman::p_load(tidyverse, magrittr, readxl, see, conflicted)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::filter)

flea_tbl <- read_excel("data/flea_dog_cat_fox_site.xlsx") |> 
  mutate(animal = as_factor(animal))

stat_1fac_tbl <- flea_tbl |> 
  filter(site == "city") |> 
  group_by(animal) |> 
  summarise(mean = mean(jump_length),
            sd = sd(jump_length),
            se = sd/sqrt(n()))

ggplot(stat_1fac_tbl, aes(x = animal, y = mean, fill = animal)) + 
  theme_minimal() +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2)

stat_2fac_tbl <- flea_tbl |> 
  group_by(animal, site) |> 
  summarise(mean = mean(jump_length),
            sd = sd(jump_length),
            se = sd/sqrt(n()))

ggplot(stat_2fac_tbl, aes(x = animal, y = mean, fill = site)) + 
  theme_minimal() +
  geom_bar(stat = "identity", width = 0.75,
           position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2,  
                position = position_dodge(width = 0.9, preserve = "single")) +
  geom_text(aes(label = c("a"), 
                y = mean + sd + 0.1),  
            position = position_dodge(width = 0.9), vjust = -0.25)

