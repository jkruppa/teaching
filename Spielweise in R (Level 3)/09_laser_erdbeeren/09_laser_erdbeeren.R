## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
pacman::p_load(tidyverse, fs, readxl,
               janitor, parameters, magrittr,
               broom, plyr, conflicted)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(magrittr::set_names)

## laden der spektren
berry_files <- list.files("C:/Users/jokruppa/work/consulting/Mansfeld_Niklas/Erdbeeren",
                          pattern = "^E", full.names = TRUE)



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

## laden des zuckergehalts

sugar_tbl <- read_excel("Zucker Erdbeeren 2.05.23.xlsx") %>% 
  clean_names() %>% 
  select(-brixwert, -brix_mittel_note, -messwiederholung,
         -g_zucker_l_saft_mittel_note, -oe_einzelfrucht) %>% 
  filter(!is.na(brix_einzelfrucht)) %>% 
  mutate(E = str_c("E_", boniturnote, ".", fruchtnummer))

## beide Datensätze zusammen
berry_sugar_tbl <- left_join(berry_tbl, sugar_tbl,
                             by = c("E" = "E")) %>% 
  #filter(boniturnote %in% c(1, 5)) %>% 
  mutate(boniturnote = as_factor(boniturnote))


wave_vec <- berry_sugar_tbl %>% pull(wave) %>% unique()

length(wave_vec)

berry_sugar_tbl %>% 
  filter(wave == 841) %>% 
  ggplot(aes(x = mean, y = g_zucker_l_saft, color = boniturnote)) +
  theme_bw() +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) #+
 # facet_wrap(~ wave)

berry_sugar_tbl %>% 
  filter(wave == 231) %>% 
  ggplot(aes(x = brix_einzelfrucht, y = mean)) +
  theme_bw() +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ wave)

berry_sugar_tbl %>% 
  filter(wave == 231) %$% 
  lm(mean ~ brix_einzelfrucht) %>% 
  glance() %>% 
  pull(r.squared)

rsquare_vec <- map_dbl(wave_vec, function(x){
  rsquare <- berry_sugar_tbl %>% 
    filter(wave == x) %$% 
    lm(brix_einzelfrucht ~ mean + boniturnote) %>%
    glance() %>% 
    pull(adj.r.squared)
  return(rsquare)
}, .progress = TRUE) %>% 
  set_names(wave_vec) 

which.max(rsquare_vec)

rsquare_vec[15]

berry_sugar_tbl %>% 
  filter(wave == wave_vec[15]) %>% 
  ggplot(aes(x = mean, y = brix_einzelfrucht,
             color = boniturnote)) +
  theme_bw() +
  geom_point() +
  geom_text(aes(label = E)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ wave)


