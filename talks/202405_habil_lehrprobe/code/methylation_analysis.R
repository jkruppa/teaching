## -----------------------------------------------------------------------------
## CAUTION: The installation of the packages will take a bunch (!) of time... 

pacman::p_load(tidyverse, IlluminaHumanMethylation450kanno.ilmn12.hg19)

## -----------------------------------------------------------------------------
## Data
## Go to the source of the package and download the *.tar.gz
## https://www.bioconductor.org/packages/release/workflows/html/methylationArrayAnalysis.html

dataDirectory <- file.path("/Users/kruppajo/Downloads/methylationArrayAnalysis/inst/extdata")
# list the files
list.files(dataDirectory, recursive = TRUE)

## -----------------------------------------------------------------------------
## Annotation

ann450k <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)
head(ann450k)

## -----------------------------------------------------------------------------
## Loading the data

targets <- read.metharray.sheet(dataDirectory, pattern = "SampleSheet.csv")
targets

rgSet <- read.metharray.exp(targets = targets)
rgSet
