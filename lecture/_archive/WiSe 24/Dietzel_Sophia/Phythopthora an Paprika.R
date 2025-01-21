pacman::p_load(tidyverse, readxl, parameters,
               janitor, see, patchwork, agridat, desplot,
               effectsize, magrittr, multcomp,
               multcompView, rcompanion, conflicted, emmeans)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)


#Noch ein anderer Datensatz den ich unter folgendem Link finde: 
# https://kwstat.github.io/agridat/reference/gumpertz.pepper.html
#Der verwendete Code ist von dem Datensatz draper.safflower.uniformity 
#angepasst und verwendet neue Pakete: agridat und desplot

data(gumpertz.pepper)
view(gumpertz.pepper)
pepper_tbl <- gumpertz.pepper

#Formieren der Daten 
Field1 <- subset(gumpertz.pepper, field=="F1")
#Problem hier gehabt: Habe den Code aus einemanderen Sheet übernommen und den Datensatz nicht geändert 
Field2 <- subset(gumpertz.pepper, field=="F2")

#Erstellen eines Planes für F1 für den Wassergehalt der einzelnen Parzellen
desplot(Field1, water~quadrat*row,
        flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
        main='Field 1')
#Erstellen eines Planes für F1 für den Befall der einzelnen Parzellen
desplot(Field1, disease~quadrat*row,
        flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
        main='Field 1', col.regions = c("blue", "red"))

desplot(Field1, leaf~quadrat*row,
        flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
        main='Field 1')

library(gridExtra)

p1 <- desplot(Field1, disease~quadrat*row,
        flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
        main='Field 1', col.regions = c("blue", "red"))

p2 <- desplot(Field2, disease~quadrat*row,
        flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
        main='Field 2', col.regions = c("blue", "red"))

grid.arrange(p1, p2, ncol = 2)


p1 <- desplot(Field1, leaf~quadrat*row,
              flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
              main='Field 1')

p2 <- desplot(Field2, leaf~quadrat*row,
              flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
              main='Field 2')

grid.arrange(p1, p2, ncol = 2)


p1 <- desplot(Field1, water~quadrat*row,
              flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
              main='Field 1')

p2 <- desplot(Field2, water~quadrat*row,
              flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
              main='Field 2')

grid.arrange(p1, p2, ncol = 2)

#Erstellen eines Planes für F2 für den Befall einzelner Parzellen
desplot(Field2, water~quadrat*row,
        flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
        main="Field 2")

#Erstellen eines Planes für F2 für den Befall der einzelnen Parzellen
desplot(Field2, disease~quadrat*row,
        flip=TRUE, tick=TRUE, aspect=20/20, # true aspect
        main="Field 2")


pacman::p_load(tidyverse, magrittr, broom,
               readxl, conflicted)
conflicts_prefer(magrittr::set_names)

lm(leaf ~ water, data = Field2) |> 
  coef()

#Scatterplot zur Visualisierung
ggplot(Field2, aes(water, leaf)) +
  theme_minimal() +
  geom_point() +
  stat_smooth()
    
ggplot(Field2, aes(disease, leaf)) +
  theme_minimal() +
  geom_jitter() +
  stat_summary(fun.y = "mean", geom = "point",
               color = "red", size = 5)
    
    