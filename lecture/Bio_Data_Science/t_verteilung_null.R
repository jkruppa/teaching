library(tidyverse)

set.seed(20201021)

T_vec <- map_dbl(1:100000, function(...){
  dog_vec <- rnorm(n = 4, mean = 8.48, sd = 0.57)
  cat_vec <- rnorm(n = 4, mean = 8.48, sd = 0.57)
  s_p <- (sd(cat_vec) + sd(dog_vec))/2 
  T_calc <- (mean(cat_vec) - mean(dog_vec))/(s_p * sqrt(2/4)) 
  return(T_calc)  
}) |> round(2)

T_vec |> magrittr::extract(1:1000) |> sort()  

(T_vec >= 3.47) |> sum()

ggplot(as_tibble(T_vec), aes(x = value)) +
  theme_minimal() +
  labs(x = "Teststatistik", y = "Anzahl") +
  geom_histogram() +
  geom_vline(xintercept = 3.47, color = "red")
