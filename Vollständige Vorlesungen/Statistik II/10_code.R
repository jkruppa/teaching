## ------------------------------------------------------------
## by J.Kruppa on Monday, June 21, 2021 (08:52)
pacman::p_load(tidyverse, magrittr, multcomp, effectsize,
               parameters, car, readxl, broom, plyr,
               gtsummary)

## Some data
one_way_tbl <- read_excel("anova_1factor.xlsx") 
two_way_tbl <- read_excel("anova_2factor.xlsx") 
p_value_tbl <- read_delim("09_pvalues.txt", delim = " ")

## ------------------------------------------------------------
## by J.Kruppa on Monday, June 21, 2021 (09:12)

one_way_tbl <- one_way_tbl %>%
  gather(treatment, weight) %>%
  mutate(treatment = as.factor(treatment)) %>% 
  na.omit

lm(weight ~ treatment, data = one_way_tbl) %>% 
  anova()

ggplot(one_way_tbl, aes(y = weight, x = treatment)) + geom_boxplot()

pairwise.t.test(x = one_way_tbl$weight, g = one_way_tbl$treatment,
                p.adjust.method = "none") 


null_tbl <- tibble(a = rnorm(10, 5, 1),
                   b = rnorm(10, 5, 1)) %>%
  gather(trt, resp) %>%
  mutate(trt = as.factor(trt))


pvalue_vec <- laply(1:100, function(...) {
  null_tbl <- tibble(a = rnorm(10, 5, 1),
                     b = rnorm(10, 5, 1)) %>%
    gather(trt, resp) %>%
    mutate(trt = as.factor(trt))
  p_value <- t.test(resp ~ trt, null_tbl, var.equal = TRUE)  %>%
    tidy %>%
    pull(p.value)
  return(p_value)
})

(pvalue_vec < 0.05) %>% sum

## mit lokalem alpha von alpha/k mit k = 100
(pvalue_vec < (0.05/100)) %>% sum

## p_adjust
(ifelse(pvalue_vec * 100 > 1, 1, pvalue_vec * 100) < 0.05) %>% sum

p.adjust(pvalue_vec)

(0.05/3)

ggplot(null_tbl, aes(x = trt, y = resp)) + geom_boxplot()

## ------------------------------------------------------------
## by J.Kruppa on Monday, June 21, 2021 (15:31)
trial %>%
  ## select(response, age, grade) %>%
  tbl_uvregression(
    method = glm,
    y = response,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2)
  ) %>%
  add_global_p() %>%  # add global p-value 
  add_nevent() %>%    # add number of events of the outcome
  add_q() %>%         # adjusts global p-values for multiple testing
  bold_p() %>%        # bold p-values under a given threshold (default 0.05)
  bold_p(t = 0.10, q = TRUE) %>% # now bold q-values under the threshold of 0.10
  bold_labels()

## ------------------------------------------------------------
## by J.Kruppa on Monday, June 21, 2021 (16:05)
## contrast matrix

contrMat(n = c("a" = 10, "b" = 10, "c" = 10, "d" = 10), type = "Dunnett")

contrMat(n = c("a" = 10, "b" = 10, "c" = 10, "d" = 10), type = "Tukey")

## ------------------------------------------------------------
## by J.Kruppa on Monday, June 21, 2021 (16:19)
## glht

## 1) Fit eines lineare Modells

trial_tbl <- trial %>%
  mutate(trt = as.factor(trt))

fit <- glm(response ~ trt + age + marker + stage + grade, data = trial_tbl,
           family = binomial)

fit %>% summary

glht(fit, linfct = mcp(stage = "Tukey")) %>%
  ## confint %>%
  tidy() %>%
  mutate(estimate = exp(estimate))

## see also here for the efffect estimates: 
## https://stats.stackexchange.com/questions/152397/interpreting-glm-interaction-contrasts-in-r-using-glht

## ------------------------------------------------------------
## by J.Kruppa on Monday, June 21, 2021 (16:45)

plot_tbl <- one_way_tbl %>%
  arrange(weight) %>%
  mutate(rank = 1:30)

plot_tbl %>%
  group_by(treatment) %>% 
  dplyr::summarise(sum_rank = sum(rank))


ggplot(plot_tbl, aes(x = treatment, y = weight)) +
  geom_boxplot()


wilcox.test(weight ~ treatment,
            data = filter(one_way_tbl, treatment %in% c("A", "B"))) 

t.test(weight ~ treatment,
       data = filter(one_way_tbl, treatment %in% c("A", "B"))) 


kruskal.test(weight ~ treatment, data = one_way_tbl) 

kruskal.test(weight ~ soil + light, data = two_way_tbl) 

anova(lm(log(weight) ~ soil + light, data = two_way_tbl))

## ------------------------------------------------------------
## by J.Kruppa on Monday, June 21, 2021 (17:08)
## ACHTUNG bei wilcox.test mit der Fallzahl!!!

low_samplesize_tbl <- tibble(a = rnorm(4, 5, 1),
                             b = c(rnorm(4, 100, 1))) %>%
  gather(trt, resp)

ggplot(low_samplesize_tbl, aes(trt, resp)) + geom_point()

t.test(resp ~ trt, data = low_samplesize_tbl)
wilcox.test(resp ~ trt, data = low_samplesize_tbl)


## ------------------------------------------------------------
## by J.Kruppa on Monday, June 21, 2021 (17:20)
## Permutationstest

perm_tbl <- filter(one_way_tbl, treatment %in% c("A", "B"))

t.test(weight ~ treatment, perm_tbl)$statistic

foo <- laply(1:10000, function(...) {
  t_value <- perm_tbl %>%
    mutate(treatment = sample(treatment)) %>%
    t.test(weight ~ treatment, data = .) %>%
    tidy %>%
    pull(statistic)
  return(t_value)
}, .progress = "text")

hist(foo)

sum(foo > 6.34)/1000

## ------------------------------------------------------------
## by J.Kruppa on Monday, June 21, 2021 (17:13)

set.seed(1)
ngroups <- 5  #number of populations
nsample <- 10  #number of reps in each
pop.means <- c(40, 45, 55, 40, 30)  #population mean length
sigma <- 3  #residual standard deviation
n <- ngroups * nsample  #total sample size
eps <- rnorm(n, 0, sigma)  #residuals
x <- gl(ngroups, nsample, n, lab = LETTERS[1:5])  #factor
means <- rep(pop.means, rep(nsample, ngroups))
X <- model.matrix(~x - 1)  #create a design matrix
y <- as.numeric(X %*% pop.means + eps)
data <- data.frame(y, x)
head(data)  #print out the first six rows of the data set




## Pairwise t.test()

pairwise.t.test()


glht(fit, linfct = mcp(treatment = "Dunnett")) %>% tidy

contrMat(n = c(10, 10, 10), type = "Dunnett")
