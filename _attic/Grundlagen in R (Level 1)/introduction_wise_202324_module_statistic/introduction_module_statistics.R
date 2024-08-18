
### 1. Kernkonzept: Zahlen zu Vektoren über c()
## Vektorschreibweise
c(1, 3, 4, 2, 7, 9) 

## Code schicken über STRG/CTRL ENTER

## Zahlen in einem Objekt speichern

flea_count <- c(1, 3, 4, 2, 7, 9) 

### 2. Kernkonzept: Wir nutzen Objekte zum rechen

## Zuweisungspfeil <- (Windows: ALT -, Mac: option -)  

## Mit Objekten rechnen

flea_count*2

## Vergleich Objekt vs. Zahlenvektor

mean(flea_count)
mean(c(1, 3, 4, 2, 7, 9) )

## Erstes Pseudo Automatisieren

flea_count <- c(1, 3, 4, 2, 7, 9, 10) 
mean(flea_count)
sd(flea_count)

### 3. Kernkonzept: Packages und Libraries
# install.packages("tidyverse") Einmal durchführen

library(tidyverse) ## immer laden
library(readxl)

### 4. Kernkonzept: Unterschied von Wörtern und Objekten

animal <- c("dog", "dog", "dog", "dog", "cat", "cat", "cat", "cat")

### 5. Kernkonzept: Wie speichere ich Daten?

animal <- c("dog", "dog", "dog", "dog", "cat", "cat", "cat", "cat")
jump_length <- c(8.5, 9.1, 6.2, 8.4, 2.4, 4.3, 2.4, 3.1)

## Ich speichere Daten in tibble

animal_tbl <- tibble(animal = c("dog", "dog", "dog", "dog", "cat", "cat", "cat", "cat"),
                     jump_length = c(8.5, 9.1, 6.2, 8.4, 2.4, 4.3, 2.4, 3.1))

## Arbeiten mit einem tibble

animal_tbl$animal
mean(animal_tbl$jump_length)

as_factor(animal_tbl$animal)

## Ändern der Eigenschaften einer Spalte
animal_fac_tbl <- mutate(animal_tbl, animal = as_factor(animal))
animal_fac_grp_tbl <- group_by(animal_fac_tbl, animal)
summarise(animal_fac_grp_tbl,
          mean_jump = mean(jump_length),
          sd_jump = sd(jump_length),
          var_jump = var(jump_length))

## Pipen mit %>%

animal_tbl %>%
  mutate(animal = as_factor(animal)) %>% 
  group_by(animal) %>% 
  summarise(mean_jump = mean(jump_length),
            sd_jump = sd(jump_length),
            var_jump = var(jump_length))

## Laden von Daten

library(readxl)
data_tbl <- read_excel("~/work/GitHub/teaching/Grundlagen in R (Level 1)/introduction_wise_202324_module_statistic/simple_data_sheet.xlsx")

## t-Test rechnen
t.test(jump_length ~ animal, data = data_tbl)

### 6. Kernkonzept: Visualisierung von Daten mit ggplot

ggplot(data = animal_tbl, 
       aes(x = animal, y = jump_length, fill = animal)) +
  theme_bw() +
  geom_boxplot()


  
  





## gandalf









