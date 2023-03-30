pacman::p_load(tidyverse, readxl)

design_tbl <- expand_grid(light = 1:3,
                          variety = 1:2,
                          replication = 1:4) %>% 
  mutate(light = factor(light, labels = c("200", "400", "600")),
         variety = factor(variety, labels = c("A", "B"))) 

design_tbl %>% 
  slice_sample(prop = 1) %>% 
  mutate(id = 1:n())

design_tbl %>% 
  write_csv2("experimental_design.csv")  

nrow(design_tbl)

1:12 %>% sample() %>% matrix(nrow = 3, ncol = 4)


