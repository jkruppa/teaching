## -----------------------------------------------------------------------------
## CAUTION: The installation of the packages will take a bunch (!) of time... 

pacman::p_load(tidyverse, 
               minfi,
               minfiData,
               IlluminaHumanMethylation450kanno.ilmn12.hg19,
               IlluminaHumanMethylation450kmanifest,
               limma)

options(matrixStats.useNames.NA = "deprecated")

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

## read in the idat files
rgSet <- read.metharray.exp(targets = targets)
rgSet

# give the samples descriptive names
targets$ID <- paste(targets$Sample_Group, targets$Sample_Name, sep=".")
sampleNames(rgSet) <- targets$ID
rgSet

## -----------------------------------------------------------------------------
## Quality control
detP <- detectionP(rgSet)

## -> okay... what can we do?

## go on wuth QC report
qcReport(rgSet, sampNames = targets$ID, sampGroups = targets$Sample_Group, 
         pdf = "qcReport.pdf")

## Normalization

mSetSq <- preprocessQuantile(rgSet) 
mSetFn <- preprocessFunnorm(rgSet) 
mSetIl <- preprocessSWAN(rgSet)
mSetRaw <- preprocessRaw(rgSet)


