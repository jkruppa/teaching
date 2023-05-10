pacman::p_load(tidyverse, readxl, parameters,
               janitor, see, patchwork,
               effectsize, magrittr, multcomp,
               multcompView, rcompanion, conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

gurke_tbl <- read_excel("wachstum_gurke.xlsx") %>% 
  clean_names() %>% 
  mutate(versuchsgruppe = as_factor(versuchsgruppe),
         erntegewicht = ifelse(erntegewicht == 0, yes = NA, no = erntegewicht))

gurke_ernte_tbl <- gurke_tbl %>% 
  filter(versuchsgruppe != "Tageslänge") 

## Ist das Outcome annährend normalverteilt
## -> ja, ANOVA
## -> nein, Sie unten

ggplot(gurke_ernte_tbl, aes(versuchsgruppe, erntegewicht)) +
  theme_bw() +
  geom_point()

## Wir nehmen an, dass es sich bei Frischmasse um einen annährend
## normalverteilten Endpunkt handelt.

fit <- lm(erntegewicht ~ versuchsgruppe, data = gurke_ernte_tbl)

fit %>% 
  anova() %>% 
  parameters()

fit %>% 
  eta_squared()

## posthoc Test durchführen. 
gurke_ernte_tbl %$%
  pairwise.t.test(erntegewicht, versuchsgruppe,
                  pool.sd = TRUE, 
                  p.adjust.method = "none")


gurke_ernte_tbl %$%
  pairwise.t.test(erntegewicht, versuchsgruppe,
                  pool.sd = TRUE, 
                  p.adjust.method = "none") %>% 
  extract2("p.value") %>% 
  fullPTable() %>% 
  multcompLetters()


## Barplot mit compact letter display und abspeichern

stat_tbl <- gurke_ernte_tbl %>% 
  group_by(versuchsgruppe) %>% 
  summarise(mean = mean(erntegewicht, na.rm = TRUE),
            sd = sd(erntegewicht, na.rm = TRUE),
            se = sd/sqrt(n()))

ggplot(stat_tbl, aes(x = versuchsgruppe, y = mean, 
                     fill = versuchsgruppe)) + 
  theme_bw() +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.2) +
  labs(x = "Versuchsgruppe", y = "Erntegewicht in [g]") +
  theme(legend.position = "none") +
  annotate("text", 
           x = 1:6, 
           y = c(635, 635, 110, 110, 260, 260), 
           label = c("a", "a", "b", "b", "c", "c")) +
  annotate("text", x = 1.5, y = 700,
           label = "ANOVA = <0.001", size = 3) +
  scale_fill_okabeito()

ggsave("img/barplot_erntegewicht.png", 
       width = 5, height = 3)

## Zeitpunkte

gurke_time_tbl <- gurke_tbl %>% 
  filter(versuchsgruppe != "Tageslänge") %>% 
  select(-pfl, -erntegewicht) %>% 
  pivot_longer(cols = t1:t17,
               values_to = "gewicht",
               names_to = "time") %>% 
  mutate(time_fct = as_factor(time),
         time_num = as.numeric(time_fct))

ggplot(gurke_time_tbl, aes(time_num, gewicht, color = versuchsgruppe)) +
  geom_point() +
  stat_summary(fun = "mean", fun.min = "min", fun.max = "max", geom = "line") 


gurke_time_tbl <- gurke_tbl %>% 
  filter(versuchsgruppe != "Tageslänge") %>% 
  select(-pfl, -erntegewicht) %>% 
  pivot_longer(cols = t1:t17,
               values_to = "gewicht",
               names_to = "time") %>% 
  mutate(time_fct = as_factor(time),
         time_num = as.numeric(time_fct),
         gewicht = ifelse(gewicht == 0, yes = NA, no = gewicht)) #%>% 
  #filter(versuchsgruppe == "Proloog L")

ggplot(gurke_time_tbl, aes(time_num, gewicht, color = versuchsgruppe)) +
  geom_point() +
  stat_summary(fun = "mean", fun.min = "min", fun.max = "max", geom = "line") 


ggplot(gurke_time_tbl, aes(time_num, gewicht, color = versuchsgruppe)) +
  theme_bw() +
  geom_jitter(position=position_dodge(0.3), shape = 4) +
  stat_summary(fun.data="mean_sdl", , fun.args = list(mult = 1), 
               geom="pointrange", position=position_dodge(0.3))  +
  stat_summary(fun = "mean", fun.min = "min", fun.max = "max", geom = "line",
               position=position_dodge(0.3)) 


lm(gewicht ~ versuchsgruppe + time + versuchsgruppe:time, gurke_time_tbl) %>% 
  anova()

## barplots

gurke_time_tbl %>% 
  filter(time_fct == "t14") %$%
  pairwise.t.test(gewicht, versuchsgruppe,
                  pool.sd = FALSE, 
                  p.adjust.method = "bonferroni") %>% 
  extract2("p.value") %>% 
  fullPTable() %>% 
  multcompLetters()

stat_tbl <- gurke_time_tbl %>% 
  group_by(versuchsgruppe, time_fct) %>% 
  summarise(mean = mean(gewicht, na.rm = TRUE),
            sd = sd(gewicht, na.rm = TRUE),
            se = sd/sqrt(n()),
            cld_pos = mean + sd + 2)

p1 <- ggplot(stat_tbl, aes(x = time_fct, y = mean, 
                     fill = versuchsgruppe)) + 
  theme_bw() +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.8, position = position_dodge(0.9)) +
  labs(x = "Zeitpunkt", fill = "Versuchsgruppe", y = "Erntegewicht in [g]") +
  annotate("text", x = 2, y = 30,
           label = "ANOVA = <0.001", size = 3) +
  theme(legend.position = "top") +
  scale_fill_okabeito()

stat_t14_tbl <- stat_tbl %>% 
  filter(time_fct == "t14")

p2 <- ggplot(stat_t14_tbl, aes(x = time_fct, y = mean, 
                       fill = versuchsgruppe)) + 
  theme_bw() +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                width = 0.8, position = position_dodge(0.9)) +
  labs(x = "Zeitpunkt", fill = "Versuchsgruppe", y = "Erntegewicht in [g]") +
  annotate("text", 
           x = c(0.6, 0.775, 0.95, 1.075, 1.25, 1.45), 
           y = stat_t14_tbl$cld_pos, 
           label = c("a", "b", "bc", "c", "bc", "bc")) +
  theme(legend.position = "none") +
  scale_fill_okabeito()


p1 + p2 + 
  plot_layout(widths = c(7, 1))

ggsave("img/time_barplot.png", 
       width = 8, height = 5)

## Tageslängen

day_length_vec <- gurke_tbl %>% 
  filter(versuchsgruppe == "Tageslänge") %>% 
  select(t1:t17) %>% 
  as_vector()

time_daylength_tbl <- tibble(time = str_c("t", 1:17),
                             daylength = day_length_vec)

gurke_time_tbl <- gurke_tbl %>% 
  filter(versuchsgruppe != "Tageslänge") %>% 
  select(-pfl, -erntegewicht) %>% 
  pivot_longer(cols = t1:t17,
               values_to = "gewicht",
               names_to = "time") %>% 
  left_join(time_daylength_tbl, by = "time")


ggplot(gurke_time_tbl, aes(x = daylength, gewicht, color = versuchsgruppe)) +
  theme_bw() +
  geom_jitter(position=position_dodge(0.3), shape = 4) +
  stat_summary(fun.data="mean_sdl", , fun.args = list(mult = 1), 
               geom="pointrange", position=position_dodge(0.3))  +
  stat_summary(fun = "mean", fun.min = "min", fun.max = "max", geom = "line",
               position=position_dodge(0.3)) 
