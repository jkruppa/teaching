pacman::p_load(tidyverse, magrittr, agricolae, dae, desplot,
               janitor, FielDHub,
               conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(magrittr::set_names)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


three_fct_long_tbl <- expand_grid(location = 1:3, number = 1:5, plant = 1:4) |> 
  mutate(location = factor(location, labels = str_c("Location ", 1:3)),
         number = factor(number, labels = str_c("number_", 1:5)),
         plant = factor(plant, label = str_c("plant_", 1:4))) |> 
  group_by(location)  |> 
  mutate(number = sample(number)) # Randomisierung fert # Randomisierung soil
three_fct_long_tbl
view(three_fct_long_tbl)

three_fct_plot_tbl <- three_fct_long_tbl |> 
  bind_cols(expand_grid(rep = 1:3, cols = 1:4, rows = 1:5))

desplot(location ~ cols + rows | location, flip = TRUE,
        out1 = rows, out1.gpar = list(col = "grey", lty = 1),
        out2 = cols, out2.gpar = list(col = "grey", lty = 1), 
        text = plant, cex = 1, shorten = "no", 
        col = number, 
        data = three_fct_plot_tbl ,
        main = "Randomized complete block design (3-faktoriell)",
        show.key = TRUE)

#Versuch die Pflanzen richtig zuzuordnen
three_fct_long_tbl <- expand_grid(location = 1:3, number = 1:20, plant = 1:4) |> 
  mutate(location = factor(location, labels = str_c("Location ", 1:3)),
         number = factor(number, label = str_c("number ", 1:20)),
         plant = factor(plant, labels = str_c("plant ", 1:4))) |> 
  group_by(location)  |> 
  mutate(number = sample(number)) # Randomisierung fert # Randomisierung soil
three_fct_long_tbl
view(three_fct_long_tbl)

three_fct_plot_tbl <- three_fct_long_tbl |> 
  bind_cols(expand_grid(rep = 1:3, cols = 1:4, rows = 1:5))

desplot(location ~ cols + rows | location, flip = TRUE,
        out1 = rows, out1.gpar = list(col = "grey", lty = 1),
        out2 = cols, out2.gpar = list(col = "grey", lty = 1), 
        text = plant, cex = 1, shorten = "no", 
        col = number, 
        data = three_fct_plot_tbl ,
        main = "Randomized complete block design (3-faktoriell)",
        show.key = TRUE)

