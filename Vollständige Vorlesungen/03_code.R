## ------------------------------------------------------------
## by J.Kruppa on Monday, November 16, 2020 (13:32)
pacman::p_load(vcd, tidyverse, corrplot, mosaic)

## p_load()
## library("package") & install.package("package")

## wobin ich?
getwd()

wrong_window_path <- "C:\\Users\\kruppajo\\Desktop\\20201116"
right_window_path <- "C:/Users/kruppajo/Desktop/20201116"

## was habe ich?
data_file <- dir(full.names = TRUE, pattern = ".csv$")

## datei einlesen
data_tbl <- read_csv(data_file) %>%
  mutate(gender = as.factor(gender))

## is.factor / as.factor
## is.numeric / as.numeric
## is.character / as.character

data_tbl$gender %>% is.numeric 
data_tbl$type %>% is.character

## datei einlesen
data_tbl <- read_csv(data_file) %>%
  mutate(gender = as.factor(gender),
         type = as.factor(type),
         vitality = as.ordered(vitality))

data_tbl$vitality %>% tally

data_tbl <- read_csv(data_file) %>%
  mutate_if(is.character, as.factor)

data_cont_tbl <- data_tbl %>%
  select_if(is.numeric)


## ------------------------------------------------------------
## by J.Kruppa on Monday, November 16, 2020 (16:44)

pdf("weight_jump.pdf", width = 8, height = 8)
ggplot(data = data_tbl, aes(x = weight, y = jump_length,
                            color = type, shape = gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw()
dev.off()

pdf("weight_jump.pdf", width = 8, height = 8)
ggplot(data = data_tbl, aes(x = weight, y = jump_length)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw()
dev.off()


pdf("weight_jump.pdf", width = 8, height = 8)
ggplot(data = data_tbl, aes(x = weight, y = jump_length)) +
  geom_boxplot(alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw()
dev.off()

## categorisieren weil wir es können
data_tbl <- read_csv(data_file) %>%
  mutate(gender = as.factor(gender),
         type = as.factor(type),
         vitality = as.ordered(vitality),
         weight_bin = ifelse(weight < 3, "low", "high"),
         weight_cat = case_when(weight <= 2 ~ "low",
                                weight > 2 & weight <= 3 ~ "mid",
                                weight > 3 ~ "high"),
         weight_bin = factor(weight_bin,
                             levels = c("low", "high"),
                             labels = c("niedriges Gewicht", "hohes Gewicht")),
         weight_cat = ordered(weight_cat,
                              levels = c("low", "mid", "high")))



pdf("weight_jump.pdf", width = 8, height = 8)
ggplot(data = data_tbl, aes(x = weight_cat, y = jump_length,
                            fill = weight_cat)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(alpha = 0.5) +
  ## geom_smooth(method = lm, se = FALSE) +
  theme_bw()
dev.off()


data_tbl %>%
  select(gender, weight_bin) %>%
  table

pdf("weight_jump.pdf", width = 8, height = 8)
mosaic(~ type + vitality, data = data_tbl, shade = TRUE)
dev.off()

data_tbl %>%
  filter(gender == "male" & weight_cat == "low")


M <- data_tbl %>%
  select_if(is.numeric) %>%
  cor()

pdf("weight_jump.pdf", width = 8, height = 8)
corrplot(M, method = "ellipse")
dev.off()

corrplot.mixed(M, lower = "ellipse", upper = "number")









## ------------------------------------------------------------
## by J.Kruppa on Monday, November 16, 2020 (13:32)
foo <- read_csv("jump_length.csv") ## %>%
  ## mutate_if(is.character, as.factor)


library(vcd)
mosaic(~ gender + weight, data = foo, shade = TRUE)


M <- foo %>%
  select_if(is.numeric) %>%
  cor()

M <- cor(foo)
corrplot(M, method = "ellipse")

corrplot.mixed(M, lower = "ellipse", upper = "number")

