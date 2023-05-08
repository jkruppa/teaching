## ------------------------------------------------------------
## Please visit https://jkruppa.github.io/programing-preface.html 
## for more information on R programing.
## ------------------------------------------------------------

## load packages, please install first the R package pacman
pacman::p_load(tidyverse, readxl, fs, parameters,
               effectsize, magrittr, multcomp,
               multcompView, rcompanion, conflicted)
conflicts_prefer(dplyr::select)

basi_tbl <- read_excel("keimversuch_basilikum.xlsx") %>% 
  mutate(versuchsgruppe = as_factor(versuchsgruppe))

## Ist das Outcome annährend normalverteilt?
## -> ja, ANOVA

ggplot(basi_tbl, aes(versuchsgruppe, frischmasse)) +
  theme_bw() +
  geom_boxplot() +
  geom_jitter(color = "red") +
  geom_hline(yintercept = 23.06, color = "blue")

fit <- lm(frischmasse ~ versuchsgruppe, data = basi_tbl)

fit %>% 
  anova() %>% 
  parameters()

fit %>% 
  eta_squared()

## posthoc Test durchführen. 
## Welcher paarweise Unterschied ist signifikant?

## alle Varianzen in allen Gruppen sind gleich
basi_tbl %$%
  pairwise.t.test(frischmasse, versuchsgruppe)

## alle Varianzen in allen Gruppen unterscheiden sich
basi_tbl %$%
  pairwise.t.test(frischmasse, versuchsgruppe,
                  pool.sd = FALSE)

## Soll ich für multiple Vergleiche adjustieren?
## -> nein!

basi_tbl %$%
  pairwise.t.test(frischmasse, versuchsgruppe,
                  pool.sd = FALSE, p.adjust.method = "none")

## -> ja

basi_tbl %$%
  pairwise.t.test(frischmasse, versuchsgruppe,
                  pool.sd = FALSE, 
                  p.adjust.method = "bonferroni")

## Barplot mit compact letter display

basi_tbl %$%
  pairwise.t.test(frischmasse, versuchsgruppe,
                  pool.sd = FALSE, 
                  p.adjust.method = "bonferroni") %>% 
  extract2("p.value") %>% 
  fullPTable() %>% 
  multcompLetters()

stat_tbl <- basi_tbl %>% 
  group_by(versuchsgruppe) %>% 
  summarise(mean = mean(frischmasse),
            sd = sd(frischmasse),
            se = sd/sqrt(n()))

ggplot(stat_tbl, aes(x = versuchsgruppe, y = mean, 
                     fill = versuchsgruppe)) + 
  theme_bw() +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2) +
  labs(x = "Versuchsgruppe", y = "Frischmasse in [g]") +
  theme(legend.position = "none") +
  annotate("text", 
           x = c(1, 2, 3, 4), 
           y = c(19, 31, 27, 37), 
           label = c("a", "bc", "b", "c")) +
  annotate("text", x = 1, y = 35,
           label = "ANOVA: p = <0.001", size = 3)
ggsave("img/barplot_frischmasse.png", 
       width = 5, height = 3)

## Trockengewicht

fit <- lm(trockenmasse ~ versuchsgruppe, data = basi_tbl)

fit %>% 
  anova() %>% 
  parameters()

fit %>% 
  eta_squared()

basi_tbl %$%
  pairwise.t.test(trockenmasse, versuchsgruppe,
                  pool.sd = FALSE, 
                  p.adjust.method = "bonferroni") %>% 
  extract2("p.value") %>% 
  fullPTable() %>% 
  multcompLetters()


## Barplot mit compact letter display und abspeichern

stat_tbl <- basi_tbl %>% 
  group_by(versuchsgruppe) %>% 
  summarise(mean = mean(trockenmasse),
            sd = sd(trockenmasse),
            se = sd/sqrt(n()))

ggplot(stat_tbl, aes(x = versuchsgruppe, y = mean, 
                     fill = versuchsgruppe)) + 
  theme_bw() +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2) +
  labs(x = "Versuchsgruppe", y = "Trockenmasse in [g]") +
  theme(legend.position = "none") +
  annotate("text", 
           x = 1:4, 
           y = c(1.5, 2.2, 1.9, 2.7), 
           label = c("a", "bc", "b", "c")) +
  annotate("text", x = 1, y = 3,
           label = "ANOVA = <0.001", size = 3)

ggsave("img/barplot_trockenmasse.png", 
       width = 5, height = 3)

##

basi_time_tbl <- basi_tbl %>% 
  select(versuchsgruppe, t1:t4) %>% 
  pivot_longer(cols = t1:t4,
               values_to = "values",
               names_to = "timepoint") %>% 
  mutate(timepoint = as_factor(timepoint),
         time_num = as.numeric(timepoint))

ggplot(basi_time_tbl, aes(time_num, values, color = versuchsgruppe)) +
  geom_point() +
  stat_summary(fun = "mean", fun.min = "min", fun.max = "max", geom = "line") 

ggplot(basi_time_tbl, aes(time_num, values, color = versuchsgruppe)) +
  theme_bw() +
  geom_jitter(position=position_dodge(0.3), shape = 4) +
  stat_summary(fun.data="mean_sdl", , fun.args = list(mult = 1), 
               geom="pointrange", position=position_dodge(0.3))  +
  stat_summary(fun = "mean", fun.min = "min", fun.max = "max", geom = "line",
               position=position_dodge(0.3)) 


lm(values ~ versuchsgruppe + timepoint + versuchsgruppe:timepoint, basi_time_tbl) %>% 
  anova()









