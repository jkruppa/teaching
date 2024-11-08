library(tidyverse)
library(readxl)
library(emmeans)
library(multcomp)

spinat_tbl <- read_excel("Projektmanagement_Auswertung.xlsx") %>% 
  mutate(Versuchsgruppe = as_factor(Versuchsgruppe),
         Block = as_factor(Block)) %>% 
  select(Versuchsgruppe, Block, 
         nicht_auf = `nicht aufgelaufen4`,
         auf = aufgelaufen4,
         tot = `nach dem Auflaufen abgestorben4`) %>% 
  mutate(tot_perc = (nicht_auf + tot)/50,
         auf_perc = auf/50)

spinat_tbl %>% 
  ggplot(aes(x = Versuchsgruppe, y = auf_perc,
             color = Block, shape = Block)) +
  theme_minimal() +
  geom_point(size = 4) +
  ylim(0, 1)

spinat_fit <- lm(auf_perc ~ Versuchsgruppe, data = spinat_tbl)

spinat_fit %>% anova()

spinat_fit %>% 
  emmeans(~ Versuchsgruppe, vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters, adjust = "bonferroni")


