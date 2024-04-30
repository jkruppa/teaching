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

## -----------------------------------------------------------------------------
## Quality control

detP <- detectionP(rgSet)
head(detP)

# remove poor quality samples
keep <- colMeans(detP) < 0.05
rgSet <- rgSet[,keep]
rgSet

# remove poor quality samples from targets data
targets <- targets[keep,]
targets[,1:5]

# remove poor quality samples from detection p-value table
detP <- detP[,keep]
dim(detP)

## -----------------------------------------------------------------------------
## Normalisation

mSetSq <- preprocessQuantile(rgSet) 
mSetRaw <- preprocessRaw(rgSet)

# visualise what the data looks like before and after normalisation
par(mfrow=c(1,2))
densityPlot(rgSet, sampGroups=targets$Sample_Group,main="Raw", legend=FALSE)
legend("top", legend = levels(factor(targets$Sample_Group)), 
       text.col=brewer.pal(8,"Dark2"))
densityPlot(getBeta(mSetSq), sampGroups=targets$Sample_Group,
            main="Normalized", legend=FALSE)
legend("top", legend = levels(factor(targets$Sample_Group)), 
       text.col=brewer.pal(8,"Dark2"))

## -----------------------------------------------------------------------------
## Data exploration

# MDS plots to look at largest sources of variation
par(mfrow=c(1,2))
plotMDS(getM(mSetSq), top=1000, gene.selection="common", 
        col=pal[factor(targets$Sample_Group)])
legend("top", legend=levels(factor(targets$Sample_Group)), text.col=pal,
       bg="white", cex=0.7)

plotMDS(getM(mSetSq), top=1000, gene.selection="common",  
        col=pal[factor(targets$Sample_Source)])
legend("top", legend=levels(factor(targets$Sample_Source)), text.col=pal,
       bg="white", cex=0.7)

## -----------------------------------------------------------------------------
## Further filtering...

# ensure probes are in the same order in the mSetSq and detP objects
detP <- detP[match(featureNames(mSetSq),rownames(detP)),] 

# remove any probes that have failed in one or more samples
keep <- rowSums(detP < 0.01) == ncol(mSetSq) 
table(keep)

mSetSqFlt <- mSetSq[keep,]
mSetSqFlt

# exclude cross reactive probes 
xReactiveProbes <- read.csv(file=paste(dataDirectory,
                                       "48639-non-specific-probes-Illumina450k.csv",
                                       sep="/"), stringsAsFactors=FALSE)
keep <- !(featureNames(mSetSqFlt) %in% xReactiveProbes$TargetID)
table(keep)

# remove probes with SNPs at CpG site
mSetSqFlt <- dropLociWithSnps(mSetSqFlt)


## -----------------------------------------------------------------------------
## Probe-wise differential methylation analysis


# calculate M-values for statistical analysis
mVals <- getM(mSetSqFlt)
head(mVals[,1:5])

bVals <- getBeta(mSetSqFlt)
head(bVals[,1:5])





