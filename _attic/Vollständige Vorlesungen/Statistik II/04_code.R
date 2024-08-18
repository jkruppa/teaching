## ------------------------------------------------------------
## by J.Kruppa on Monday, May  3, 2021 (08:22)
pacman::p_load(tidyverse, mice, naniar, finalfit, Amelia,
               Hmisc, missForest, rcompanion, car, broom,
               plyr)
data_dir <- file.path("C:\\Users\\kruppajo\\Desktop\\20210503")
easyfit_file <- file.path(data_dir, "easyFit_missing_n100.csv")
birds_file <- file.path(data_dir, "birds.csv")
longnose_file <- file.path(data_dir, "longnose.csv")

## ------------------------------------------------------------
## by J.Kruppa on Monday, May  3, 2021 (09:25)
## Was ist eigentlich das Problem?

easyFit_tbl <- read_csv(easyfit_file) %>%
  prodNA(noNA = 0.1)

## rechnen eine multiple lineare gaussian regression
glm(weight ~ ., data = easyFit_tbl, family = gaussian) %>%
  summary

## ------------------------------------------------------------
## by J.Kruppa on Monday, May  3, 2021 (14:40)

data(starwars)

starwars_tbl <- starwars %>%
  select_if(Negate(is.list)) %>%
  mutate_if(is.character, as.factor)

## https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
vis_miss(starwars_tbl)

gg_miss_upset(starwars_tbl)

starwars_tbl %>% ff_glimpse()

## 1) mice

starwars_small_tbl <- starwars_tbl %>%
  select(name, height, hair_color, sex, homeworld)

method_tbl <- tibble(var_id = names(starwars_small_tbl),
                     meth = c("", "pmm", "pmm", "polyreg", "sample"))

  
temp_data <- mice(starwars_small_tbl, m = 3, maxit = 5,
                  meth = method_tbl$meth, seed = 500)

summary(temp_data)


mice::complete(temp_data, 1) %>% head
mice::complete(temp_data, 2) %>% head
mice::complete(temp_data, 3) %>% head

densityplot(temp_data)

starwars_small_tbl$hair_color %>% table
mice::complete(temp_data, 1)$hair_color %>% table

foo <- lm(weight ~ ., ) %>% summary

## weiter mit easyFit Beispiel
## https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
          
easyFit_tbl <- read_csv(easyfit_file) %>%
  select(-weight) %>%
  mutate_if(is.character, as.factor) %>%
  filter(!(is.na(weight_bin) | is.na(easyFit)))

vis_miss(easyFit_tbl)

glimpse(easyFit_tbl)

method_tbl <- tibble(var_id = names(easyFit_tbl),
                     meth = c("", "pmm", "logreg", "pmm", 
                              "", "pmm", "pmm", "pmm", "logreg"))

easyFit_imp <- mice(easyFit_tbl, m = 10, maxit = 5,
                    meth = method_tbl$meth, seed = 500)

densityplot(easyFit_imp)

xyplot(easyFit_imp, weight ~ age + calories,pch=18,cex=1)

mice::complete(easyFit_imp, 1) %>% head
mice::complete(easyFit_imp, 2) %>% head
mice::complete(easyFit_imp, 3) %>% head
mice::complete(easyFit_imp, 4) %>% head
mice::complete(easyFit_imp, 5) %>% head

modelFit1 <- with(easyFit_imp, glm(weight_bin ~ age + gender + sport +
                                     calories + height,
                                   family = binomial))

mice::pool(modelFit1) %>% summary ## %>% pull(estimate) %>% exp %>% round(2)

## model mit missing
missing_fit <- glm(weight_bin ~ age + gender + sport + calories + height,
                   family = binomial, data = easyFit_tbl)

missing_fit$na.action %>% length

## was tun wenn pool nicht geht

mice_data_lst <- llply(1:easyFit_imp$m, function(i) {
  tmp_tbl <- mice::complete(easyFit_imp, i) %>%
    as_tibble()
  out_tbl <- glm(weight_bin ~ age + gender + sport + calories + height,
                 family = binomial, data = tmp_tbl) %>% tidy %>%
             filter(term != "(Intercept)")
  return(out_tbl)  
}, .progress = "text")

reduced_df <- mice_data_lst %>%
  llply(select_if, is.numeric) %>%
  Reduce("+", .)/easyFit_imp$m 

reduced_df %>% as_tibble %>%
  select(estimate, std.error) %>%
  mutate(statistic = estimate / std.error)

## 2) Amelia

data(iris)

iris.mis <- prodNA(iris, noNA = 0.1)

amelia_fit <- amelia(iris.mis, m=5, parallel = "multicore", noms = "Species")

amelia_fit$imputations

## 3) Hmisc

with(easyFit_tbl, impute(age, mean))
## impute(easyFit_tbl$age, mean)

easyFit_tbl %>%
  mutate(age_imp = impute(age, mean),
         age_random = impute(age, 'random'),
         pet_imp = impute(pet, max)) %>% ## most frequent
  select(age, age_imp, age_random, pet, pet_imp)


impute_arg <- aregImpute(~ age + gender + sport + calories + height,
                         data = easyFit_tbl, n.impute = 5)

impute_arg$imputed

## 4) mi (veraltet)




data(starwars)

## https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
vis_miss(starwars)







foo <- starwars %>% select_if(Negate(is.list))



ff_glimpse(foo)

explanatory = c("age", "sex.factor", 
  "nodes", "obstruct.factor",  
  "smoking_mcar", "smoking_mar")
dependent = "mort_5yr"
colon_s %>% 
  missing_pairs(dependent, explanatory)

