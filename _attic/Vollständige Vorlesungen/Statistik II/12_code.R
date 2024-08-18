## ------------------------------------------------------------
## by J.Kruppa on Monday, July  5, 2021 (08:08)
pacman::p_load(tidyverse, meta, metafor, dmetar, esc,
               readxl, ranger, janitor, mosaic, pROC)

data(SuicidePrevention)

## prevent_tbl <- read_excel("SuicidePrevention.xlsx")

SuicidePrevention$n.e <- as.numeric(SuicidePrevention$n.e)
SuicidePrevention$mean.e <- as.numeric(SuicidePrevention$mean.e)
SuicidePrevention$sd.e <- as.numeric(SuicidePrevention$sd.e)
SuicidePrevention$n.c <- as.numeric(SuicidePrevention$n.c)
SuicidePrevention$mean.c <- as.numeric(SuicidePrevention$mean.c)
SuicidePrevention$sd.c <- as.numeric(SuicidePrevention$sd.c)
SuicidePrevention$n.c <- as.numeric(SuicidePrevention$n.c)
SuicidePrevention$age_group <- as.factor(SuicidePrevention$age_group)
SuicidePrevention$control <- as.factor(SuicidePrevention$control)


# Define the data we need to calculate SMD/d
grp1m <- 50   # mean of group 1
grp2m <- 60   # mean of group 2
grp1sd <- 10  # sd of group 1
grp2sd <- 10  # sd of group 2
grp1n <- 100  # n of group1
grp2n <- 100  # n of group2

# Calculate effect size
esc_mean_sd(grp1m = grp1m, grp2m = grp2m, 
            grp1sd = grp1sd, grp2sd = grp2sd, 
            grp1n = grp1n, grp2n = grp2n)


SP_calc <- esc_mean_sd(grp1m = SuicidePrevention$mean.e,
                       grp1sd = SuicidePrevention$sd.e,
                       grp1n = SuicidePrevention$n.e,
                       grp2m = SuicidePrevention$mean.c,
                       grp2sd = SuicidePrevention$sd.c,
                       grp2n = SuicidePrevention$n.c,
                       study = SuicidePrevention$author,
                       es.type = "g") %>% 
                     as.data.frame()


data(ThirdWave)

m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Author,
                 data = ThirdWave,
                 sm = "SMD",
                 comb.fixed = FALSE,
                 comb.random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")



SuicidePrevention

## ------------------------------------------------------------
## by J.Kruppa on Monday, July  5, 2021 (14:59)
## Prädiktionsmodell

titanic_tbl <- read_csv("train.csv") %>%
  clean_names() %>% 
  select(-cabin, -name, -ticket) %>%
  na.omit %>%
  mutate_if(is.character, as.factor) %>%
  mutate(died = ifelse(survived == 1, 0, 1))


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


