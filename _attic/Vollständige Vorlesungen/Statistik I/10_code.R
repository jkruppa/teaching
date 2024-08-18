## ------------------------------------------------------------
## by J.Kruppa on Monday, January 18, 2021 (17:32)
## https://cran.r-project.org/web/packages/cutpointr/vignettes/cutpointr.html
library(tidyverse)
library(cutpointr)
data(suicide)
as_tibble(suicide)

cp <- cutpointr(data = suicide,
                x = dsi,
                class = suicide, 
                method = maximize_metric, metric = sum_sens_spec)

suicide$suicide %>% table

summary(cp)

pdf("plot.pdf")
plot(cp)
dev.off()

## ------------------------------------------------------------
## by J.Kruppa on Monday, January 18, 2021 (17:42)
## https://web.expasy.org/pROC/screenshots.html
library(pROC)
