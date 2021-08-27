## ------------------------------------------------------------
## by J.Kruppa on Monday, July  5, 2021 (08:08)
pacman::p_load(tidyverse, meta, metafor, dmetar, esc,
               readxl, ranger, janitor, mosaic, pROC,
               caret, e1071)

## ------------------------------------------------------------
## by J.Kruppa on Monday, July  5, 2021 (14:59)
## Prädiktionsmodell

titanic_tbl <- read_csv("train.csv") %>%
  clean_names() %>% 
  select(-cabin, -name, -ticket) %>%
  na.omit %>%
  mutate_if(is.character, as.factor) %>%
  mutate(died = as.factor(ifelse(survived == 1, 0, 1)))


titanic_train_tbl <- titanic_tbl %>% dplyr::sample_frac(0.75)
titanic_test_tbl  <- dplyr::anti_join(titanic_tbl,
                                      titanic_train_tbl, by = 'passenger_id')

table(titanic_train_tbl$died)/sum(table(titanic_train_tbl$died))
table(titanic_test_tbl$died)/sum(table(titanic_test_tbl$died))


## fitten Sie ein Modell
fit <- glm(died ~ sex + pclass + age, titanic_train_tbl, family = binomial)

## prädiktionsmodell
pred_vec <- predict(fit, newdata = titanic_test_tbl, type = "response")

length(pred_vec)

## wandeln Sie die Klassenzugehörigkeitswahrscheinlichkeiten um
res_tbl <- tibble(pred_prob_vec = pred_vec,
                  pred_died = ifelse(pred_vec >= 0.5, 1, 0),
                  real_died = titanic_test_tbl$died)

## 2x2 tafel raus (falsche sortierung!!!)
tally(real_died ~ pred_died, res_tbl)

## wir wollen eine ROC curve

rocobj1 <- plot.roc(res_tbl$real_died, res_tbl$pred_prob_vec,
                    main="Statistical comparison", percent=TRUE, col="#1c61b6")

testobj <- roc.test(rocobj1, rocobj2)

text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))

legend("bottomright", legend=c("S100B", "NDKA"), col=c("#1c61b6", "#008600"), lwd=2)

## titanic_test_tbl <- read_csv("test.csv") %>%
##   clean_names() %>% 
##   mutate(died = ifelse(survived == 1, 0, 1))


## glm 
fit_glm <- glm(died ~ sex + pclass + age, titanic_train_tbl, family = binomial)
pred_vec_glm <- predict(fit, newdata = titanic_test_tbl, type = "response")
res_glm_tbl <- tibble(pred_prob_vec = pred_vec_glm,
                      pred_died = ifelse(pred_vec_glm >= 0.5, 1, 0),
                      real_died = titanic_test_tbl$died)

## ------------------------------------------------------------
## by J.Kruppa on Monday, July 12, 2021 (09:11)
## SVM

fit_svm <- e1071::svm(died ~ sex + pclass + age, titanic_train_tbl,
                      probability = TRUE)
pred_vec_svm <- predict(fit_svm, newdata = titanic_test_tbl,
                        probability = TRUE)

pred_vec_svm %>% str

prob_svm <- attr(pred_vec_svm, "probabilities")[, "1"]

res_svm_tbl <- tibble(pred_prob_vec = prob_svm,
                      pred_died = ifelse(prob_svm >= 0.5, 1, 0),
                      real_died = titanic_test_tbl$died)

## ------------------------------------------------------------
## by J.Kruppa on Monday, July 12, 2021 (09:11)
## k-NN

fit_knn <- caret::knn3(died ~ sex + pclass + age, titanic_train_tbl)
pred_vec_knn <- predict(fit_knn, newdata = titanic_test_tbl,
                        type = c("prob"))

prob_knn <- pred_vec_knn[, "1"]


res_knn_tbl <- tibble(pred_prob_vec = prob_knn,
                      pred_died = ifelse(prob_knn >= 0.5, 1, 0),
                      real_died = titanic_test_tbl$died)

## ------------------------------------------------------------
## by J.Kruppa on Monday, July 12, 2021 (09:12)
## random forest

fit_rf <- ranger::ranger(died ~ sex + pclass + age, titanic_train_tbl,
                         probability = TRUE)
pred_vec_rf <- predict(fit_rf, data = titanic_test_tbl)

prob_rf <- pred_vec_rf$predictions[, "1"]

res_rf_tbl <- tibble(pred_prob_vec = prob_rf,
                      pred_died = ifelse(prob_rf >= 0.5, 1, 0),
                      real_died = titanic_test_tbl$died)

## Variable importance
rg.iris <- ranger(Species ~ ., data = iris, importance = "impurity")
rg.iris$variable.importance

fit_rf <- ranger::ranger(died ~ sex + pclass + age, titanic_train_tbl,
                         importance = "impurity")
fit_rf$variable.importance


## ------------------------------------------------------------
## by J.Kruppa on Monday, July 12, 2021 (15:34)

rocobj1 <- plot.roc(res_glm_tbl$real_died,
                    res_glm_tbl$pred_prob_vec,
                    main="Statistical comparison", percent=TRUE, col="#1c61b6")
rocobj2 <- lines.roc(res_svm_tbl$real_died,
                     res_svm_tbl$pred_prob_vec, percent=TRUE, col="#008600")
rocobj3 <- lines.roc(res_knn_tbl$real_died,
                     res_knn_tbl$pred_prob_vec, percent=TRUE, col="red")
rocobj3 <- lines.roc(res_rf_tbl$real_died,
                     res_rf_tbl$pred_prob_vec, percent=TRUE, col="blue")


testobj <- roc.test(rocobj1, rocobj2)

text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))

legend("bottomright", legend=c("S100B", "NDKA"), col=c("#1c61b6", "#008600"), lwd=2)


library(pROC)

data(aSAH)



rocobj1 <- plot.roc(aSAH$outcome, aSAH$s100,

main="Statistical comparison", percent=TRUE, col="#1c61b6")

rocobj2 <- lines.roc(aSAH$outcome, aSAH$ndka, percent=TRUE, col="#008600")

testobj <- roc.test(rocobj1, rocobj2)

text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))

legend("bottomright", legend=c("S100B", "NDKA"), col=c("#1c61b6", "#008600"), lwd=2)


## ------------------------------------------------------------
## by J.Kruppa on Monday, July 12, 2021 (16:13)

data_tbl <- tibble(dog = c(8, 7.4, 6.2, 8.1, 9.1),
                   cat = c(10.1, 9.2, 8.2, 11.2, 6.2)) %>%
  gather

t.test(value ~ key, data = data_tbl, var.equal = TRUE)
