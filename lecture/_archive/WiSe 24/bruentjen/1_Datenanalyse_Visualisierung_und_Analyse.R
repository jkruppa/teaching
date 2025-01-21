# Laden der nötigen Bibliothek
library(agridat)
library(lattice) # falls nicht installiert, install.packages("lattice")
library(ggplot2) # falls nicht installiert, install.packages("ggplot2")
library(multcomp)
library(multcompView)
library(effectsize)
library(emmeans)

# Daten laden
data(crampton.pig)
dat <- crampton.pig

## j. Kruppa-Scheetz Start

pig_tbl <- crampton.pig %>% 
  as_tibble() %>% 
  mutate(delta = weight2 - weight1,
         delta_feed = delta/feed)

pig_tbl %>% 
  ggplot(aes(x = treatment, y = delta_feed)) +
  geom_point() +
  ylim(0, NA)

pig_tbl %>% 
  ggplot(aes(x = weight1, y = delta)) +
  geom_point() +
  ylim(0, NA)

lm(delta ~ weight1, data = pig_tbl) %>% 
  summary()

fit <- lm(delta_feed ~ treatment, data = pig_tbl)

fit %>% anova()

fit %>% eta_squared()

fit %>% 
  emmeans(~ treatment, vcov. = sandwich::vcovHAC) %>% 
  cld(Letters = letters) %>% 
  mutate(.group = str_trim(.group),
         sd = SE * sqrt(50)) %>% 
  ggplot(aes(x = treatment, y = emmean, fill = treatment)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin = emmean-sd, ymax = emmean+sd),
                width = 0.2,  
                position = position_dodge(width = 0.9, preserve = "single")) +
  geom_text(aes(label = .group, y = emmean + sd),  
            position = position_dodge(width = 0.9), vjust = -0.25) +
  ylim(0, 0.35) +
  theme(legend.position = "none")


## J. Kruppa-Scheetz Ende

# Gewichtszunahme berechnen
dat <- transform(dat, gain=weight2-weight1)

# Diagramm mit lattice erstellen (ursprünglich)
xyplot(gain ~ feed, dat, group=treatment, type=c('p','r'),
       auto.key=list(columns=5),
       xlab="Feed eaten", ylab="Weight gain", main="crampton.pig")

# Diagramm mit ggplot2 erstellen
ggplot(dat, aes(x = feed, y = gain, color = factor(treatment))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot von Futteraufnahme und Gewichtszunahme",
       x = "Futteraufnahme [g/Tag]",
       y = "Gewichtszunahme [kg]",
       color = "Behandlung") +
  theme_minimal()

ggplot(dat, aes(x = weight1, y = gain, color = factor(treatment))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot von Anfangsgewicht und Gewichtszunahme",
       x = "Anfangsgewicht [kg]",
       y = "Gewichtszunahme [kg]",
       color = "Behandlung") +
  theme_minimal()

# Basis-Anova ohne Kovariaten
m1 <- lm(weight2 ~ treatment + rep, data=dat)
print(anova(m1))

# Anova mit Kovariaten
m2 <- lm(weight2 ~ treatment + rep + weight1 + feed, data=dat)
print(anova(m2))

# Behandlung entfernen, um das verschachtelte Modell zu testen
m3 <- lm(weight2 ~ rep + weight1 + feed, data=dat)
print(anova(m2, m3)) # p-Wert 0.07

# Basis-Anova ohne Kovariaten für Gewichtszunahme
m4 <- lm(gain ~ treatment + rep, data = dat)
print(anova(m4))

#Anova optisch als Tabelle darstellen mit R Quarto
