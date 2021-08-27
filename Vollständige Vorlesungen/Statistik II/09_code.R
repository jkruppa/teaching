## ------------------------------------------------------------
## by J.Kruppa on Monday, June 14, 2021 (08:21)
pacman::p_load(tidyverse, magrittr, multcomp, effectsize,
               parameters, car, readxl)

## ------------------------------------------------------------
## by J.Kruppa on Monday, June 14, 2021 (08:54)
## Some data
one_way_tbl <- read_excel("anova_1factor.xlsx") 
two_way_tbl <- read_excel("anova_2factor.xlsx") 
p_value_tbl <- read_delim("09_pvalues.txt", delim = " ")

## ------------------------------------------------------------
## by J.Kruppa on Monday, June 14, 2021 (08:53)
## One way ANOVA 

one_way_tbl <- one_way_tbl %>%
  gather(treatment, weight) %>%
  mutate(treatment = as.factor(treatment)) %>% 
  na.omit

ggplot(one_way_tbl, aes(x = treatment, y = weight, fill = treatment)) +
  geom_boxplot()

fit_one <- lm(weight ~ treatment, data = one_way_tbl) 

fit_one %>% summary
fit_one %>% anova

fit_one %>% anova %>% eta_squared
fit_one %>% anova %>% cohens_f

## two way ANOVA

two_way_tbl %<>%
  mutate_if(is.character, as.factor)

ggplot(two_way_tbl, aes(x = soil, y = weight, fill = light)) +
  geom_boxplot()

fit_two <- lm(weight ~ soil + light + soil:light, data = two_way_tbl)

fit_two %>% summary
fit_two %>% anova

## ANCOVA (https://www.datanovia.com/en/lessons/ancova-in-r/)
data("anxiety", package = "datarium")
anxiety <- anxiety %>%
  select(id, group, t1, t3) %>%
  rename(pretest = t1, posttest = t3)
anxiety[14, "posttest"] <- 19


anxiety %>%
  lm(posttest ~ group + pretest + group:pretest, data = .) %>%
  anova()

anxiety %>%
  lm(posttest ~ group + pretest + group:pretest, data = .) %>%
  summary()


## ------------------------------------------------------------
## by J.Kruppa on Monday, June 14, 2021 (09:55)
## all the same: t-test() == one-way ANOVA == lm()

one_way_comp_tbl <- one_way_tbl %>%
  filter(treatment %in% c("B", "D"))

t.test(weight ~ treatment, one_way_comp_tbl, var.equal = TRUE)
lm(weight ~ treatment, one_way_comp_tbl) %>% summary
lm(weight ~ treatment, one_way_comp_tbl) %>% anova

glm(weight ~ treatment, data = one_way_comp_tbl,
    family = gaussian) %>% anova(test = "F") 
















one_way_tbl %<>%
  gather(treatment, yield) %>%
  mutate(treatment = as.factor(treatment))

leveneTest(one_way_tbl$yield, one_way_tbl$treatment)

fit <- lm(yield ~ treatment, data = one_way_tbl)

anova(fit)


## ------------------------------------------------------------
## by J.Kruppa on Monday, June 14, 2021 (09:55)
## Two way ANOVA



## ------------------------------------------------------------
## by J.Kruppa on Monday, June 14, 2021 (09:55)
## Pairwise t.test()

pairwise.t.test()


## ------------------------------------------------------------
## by J.Kruppa on Monday, June 14, 2021 (08:25)
## Multiple Testing (General Linear Hypotheses Testing)

glht(fit, linfct = mcp(treatment = "Dunnett")) %>% tidy

contrMat(n = c(10, 10, 10), type = "Dunnett")

## ------------------------------------------------------------
## by J.Kruppa on Monday, June 14, 2021 (09:55)
## all the same

one_way_comp_tbl <- one_way_tbl %>%
  gather(treatment, yield) %>%
  filter(treatment %in% c("B", "D"))

t.test(yield ~ treatment, one_way_comp_tbl, var.equal = TRUE)
lm(yield ~ treatment, one_way_comp_tbl) %>% summary
lm(yield ~ treatment, one_way_comp_tbl) %>% anova

glm(yield ~ treatment, data = one_way_comp_tbl) %>% anova(test = "Chisq") 

