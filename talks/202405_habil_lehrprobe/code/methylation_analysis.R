## -----------------------------------------------------------------------------
## CAUTION: The installation of the packages will take a bunch (!) of time... 

pacman::p_load(tidyverse, IlluminaHumanMethylation450kanno.ilmn12.hg19,
               methylationArrayAnalysis)

## 
## Data

dataDirectory <- system.file("extdata", package = "methylationArrayAnalysis")
# list the files
list.files(dataDirectory, recursive = TRUE)


## -----------------------------------------------------------------------------
## Annotation

ann450k <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)
head(ann450k)
