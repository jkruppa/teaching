pacman::p_load(tidyverse, simstudy, knitr)
## data generation
def <- defData(varname = "age", dist = "normal", formula = "60", variance = 20)
def <- defData(def, varname = "gender", formula = "0.4;0.6", dist = "categorical")
def <- defData(def, varname = "sport", formula = "120", variance = 40)
def <- defData(def, varname = "easyFit", formula = "0.5;0.25;0.25", dist = "categorical")
def <- defData(def, varname = "time_taken", dist = "poisson", formula = "30 * easyFit")
def <- defData(def, varname = "stopped_due_incident", dist = "binary", formula = "0.25")
def <- defData(def, varname = "calories", dist = "poisson", formula = "1000 + 500 * gender")
def <- defData(def, varname = "city", formula = "0.1;0.3;0.2;0.1;0.2;0.1",
               dist = "categorical")
def <- defData(def, varname = "bloodpressure", formula = "80 + 20 * gender", variance = 20)
def <- defData(def, varname = "height", formula = "140 + 20 * gender", variance = 10)
def <- defData(def, varname = "creatinin", formula = "10", variance = 2)
def <- defData(def, varname = "pet", formula = "0.8;0.2", dist = "categorical")
def <- defData(def, varname = "weight", variance = 5,
               formula = "20 + 2.5 * gender - 5 * easyFit - 30 * stopped_due_incident +
0.03 * calories + 0.05 * height - 2 * pet + 2 * gender * 2 * easyFit")
## data mutating
dt <- genData(rpois(1, 300), def)
data_tbl <- dt %>%
  as_tibble %>%
  mutate(gender = factor(gender, labels = c("woman", "man")),
         easyFit = factor(easyFit, labels = c("placebo", "dose25", "dose50")),
         city = factor(city, labels = c("Bremen", "Berlin", "Hamburg", "Hannover",
                                        "Salzburg", "Freiburg")),
         pet = factor(pet, labels = c("no", "yes")),
         weight_cat = factor(cut(weight, c(0, 55, 75, Inf)),
                             labels = c("low", "mid", "high")),
         weight_bin = factor(cut(weight, c(0, 55, Inf)),
                             labels = c("critical", "normal"))) %>%
  select(weight, weight_cat, weight_bin, everything()) %>%
  select(-id)
## data writing
write_csv(data_tbl, str_c("easyFit.csv"))
