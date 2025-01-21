library(tidyverse)
library(readxl)
library(mgcv)

heater_wide_tbl <- read_excel("Vorversuch_Heater_21_10_24.xlsx")

heater_tbl <- heater_wide_tbl %>% 
  pivot_longer(cols = Temp1:Luft8,
               names_to = "sensor",
               values_to = "measure") %>% 
  mutate(sensor = as_factor(sensor))

heater_tbl %>% 
  filter(str_detect(sensor, "Luft"),
         Nr >= 78) %>% 
  ggplot(aes(y = measure, x = Nr, color = sensor)) +
  theme_minimal() +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

heater_nr78_tbl <- heater_tbl %>% 
  filter(str_detect(sensor, "Luft"),
         Nr >= 78) %>% 
  mutate(sensor = relevel(sensor, ref = "Luft1"))

fit <- lm(measure ~ Nr + sensor, data = heater_nr78_tbl)

fit %>% summary()

gam_fit <- gam(measure ~ s(Nr) + sensor, data = heater_nr78_tbl)

gam_fit %>% summary()

heater_tbl %>% 
  filter(str_detect(sensor, "Luft"),
         Nr >= 78) %>% 
  ggplot(aes(y = measure, x = Nr, color = sensor)) +
  theme_minimal() +
  geom_point() +
  geom_line(aes(y = predict(gam_fit)), size = 1) 
  
