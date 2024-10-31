## -----------------------------------------------------------------------------
## CAUTION: The installation of the packages will take a bunch (!) of time... 

pacman::p_load(tidyverse, 
               minfi,
               IlluminaHumanMethylation450kanno.ilmn12.hg19,
               MEAL)

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
#legend("top", legend = levels(factor(targets$Sample_Group)), 
#       text.col=brewer.pal(8,"Dark2"))
densityPlot(getBeta(mSetSq), sampGroups=targets$Sample_Group,
            main="Normalized", legend=FALSE)
#legend("top", legend = levels(factor(targets$Sample_Group)), 
#       text.col=brewer.pal(8,"Dark2"))

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

# this is the factor of interest
cellType <- factor(targets$Sample_Group)
# this is the individual effect that we need to account for
individual <- factor(targets$Sample_Source) 

# use the above to create a design matrix
design <- model.matrix(~0+cellType+individual, data=targets)
colnames(design) <- c(levels(cellType),levels(individual)[-1])

# fit the linear model 
fit <- lmFit(mVals, design)
# create a contrast matrix for specific comparisons
contMatrix <- makeContrasts(naive-rTreg,
                            naive-act_naive,
                            rTreg-act_rTreg,
                            act_naive-act_rTreg,
                            levels=design)
contMatrix

# fit the contrasts
fit2 <- contrasts.fit(fit, contMatrix)
fit2 <- eBayes(fit2)

# look at the numbers of DM CpGs at FDR < 0.05
summary(decideTests(fit2))

# get the table of results for the first contrast (naive - rTreg)
ann450kSub <- ann450k[match(rownames(mVals),ann450k$Name),
                      c(1:4,12:19,24:ncol(ann450k))]
DMPs <- topTable(fit2, num=Inf, coef=1, genelist=ann450kSub)
head(DMPs)

## -> what is logFC?
## -> what is AveExpr?
## -> how to vulcano?

## -----------------------------------------------------------------------------
## Differential methylation analysis of regions

myAnnotation <- cpg.annotate(object = mVals, datatype = "array", what = "M", 
                             analysis.type = "differential", design = design, 
                             contrasts = TRUE, cont.matrix = contMatrix, 
                             coef = "naive - rTreg", arraytype = "450K")

DMRs <- dmrcate(myAnnotation, lambda=1000, C=2)
results.ranges <- extractRanges(DMRs)
results.ranges

# set up the grouping variables and colours
groups <- pal[1:length(unique(targets$Sample_Group))]
names(groups) <- levels(factor(targets$Sample_Group))
cols <- groups[as.character(factor(targets$Sample_Group))]

# draw the plot for the top DMR
par(mfrow=c(1,1))
DMR.plot(ranges = results.ranges, dmr = 2, CpGs = bVals, phen.col = cols, 
         what = "Beta", arraytype = "450K", genome = "hg19")

## -----------------------------------------------------------------------------
## Customising visualisations of methylation data

## https://www.bioconductor.org/packages/release/workflows/vignettes/methylationArrayAnalysis/inst/doc/methylationArrayAnalysis.html

## -----------------------------------------------------------------------------
## Gene ontology testing

# Get the significant CpG sites at less than 5% FDR
sigCpGs <- DMPs$Name[DMPs$adj.P.Val<0.05]
# First 10 significant CpGs
sigCpGs[1:10]

par(mfrow=c(1,1))
gst <- gometh(sig.cpg=sigCpGs, all.cpg=all, plot.bias=TRUE)

# Top 10 GO categories
topGSA(gst, number=10)

# load Broad human curated (C2) gene sets
load(paste(dataDirectory,"human_c2_v5.rdata",sep="/"))
# perform the gene set test(s)
gsa <- gsameth(sig.cpg=sigCpGs, all.cpg=all, collection=Hs.c2)

# top 10 gene sets
topGSA(gsa, number=10)

## -----------------------------------------------------------------------------
## Better with MEAL?
## https://www.bioconductor.org/packages/devel/bioc/vignettes/MEAL/inst/doc/MEAL.html

## We use here a new example

pacman::p_load(minfiData)

meth <- mapToGenome(ratioConvert(MsetEx))
rowData(meth) <- getAnnotation(meth)[, -c(1:3)]


## Remove probes measuring SNPs
meth <- dropMethylationLoci(meth)

## Remove probes with SNPs
meth <- dropLociWithSnps(meth)

## Remove probes with NAs
meth <- meth[!apply(getBeta(meth), 1, function(x) any(is.na(x))), ]


## convert example above
## meth <- preprocessRaw(rgSet)

res <- runPipeline(set = meth, variable_names = "status")

head(getAssociation(res, "DiffMean"))


resAdj <- runPipeline(set = meth, variable_names = "status", 
                      covariable_names = "age", analyses = c("DiffMean", "DiffVar"))
resAdj

## Manhattan

targetRange <- GRanges("23:13000000-23000000")
plot(resAdj, rid = "DiffMean", type = "manhattan", highlight = targetRange)

plot(resAdj, rid = "DiffMean", type = "manhattan", subset = targetRange)

## Volcano plot

plot(resAdj, rid = "DiffMean", type = "volcano", tPV = 14, tFC = 0.4, 
     show.labels = FALSE) + ggtitle("My custom Volcano")

## QQplot

plot(resAdj, rid = "DiffMean", type = "qq") + ggtitle("My custom QQplot")

## Features

plotFeature(set = meth, feat = "cg09383816", variables = "Sample_Group") + 
  ggtitle("Diff Means")

## Regional plotting

targetRange <- GRanges("chrX:13000000-14000000")
plotRegion(resAdj, targetRange)
