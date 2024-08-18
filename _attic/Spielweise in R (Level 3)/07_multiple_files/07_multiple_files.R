## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
pacman::p_load(tidyverse, fs)

berry_files <- list.files("strawberry",
                          pattern = "^E", full.names = TRUE)

read_table(berry_files[1], skip = 2, col_names = FALSE,
           col_types = cols())

berry_lst <- map(berry_files, read_table, 
                 skip = 2, col_names = FALSE, col_types = cols())

berry_lst <- map(berry_files, function(x){
  tmp_tbl <- read_table(x, 
                        skip = 2, col_names = FALSE, col_types = cols()) 
  file_name <- basename(x) %>% 
    path_ext_remove() %>% 
    str_replace_all("\\s", "_")
  tmp_tbl <- tmp_tbl %>% 
    set_names(c("wave", file_name)) 
  return(tmp_tbl)
})


berry_tbl <- berry_lst %>% 
  reduce(left_join, by = "wave") %>% 
  pivot_longer(cols = E_1.1._w1:last_col(),
               names_sep = "\\._",
               values_to = "values",
               names_to = c("E", "rep")) %>% 
  group_by(wave, E) %>% 
  summarise(mean = mean(values))


ggplot(berry_tbl, aes(wave, mean, color = E)) +
  theme_bw() +
  geom_line() +
  theme(legend.position = "none")






