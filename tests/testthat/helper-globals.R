## Fix for pheatmap partial match warning.
## https://github.com/raivokolde/pheatmap/issues/46
options(
    warnPartialMatchAttr = FALSE,
    warnPartialMatchDollar = FALSE
)

data(
    RangedSummarizedExperiment,
    SingleCellExperiment,
    matrix,
    matrix_lfc,
    package = "AcidTest",
    envir = environment()
)

## nolint start
`assay<-` <- basejump::`assay<-`
`colData<-` <- basejump::`colData<-`
`rowData<-` <- basejump::`rowData<-`
allAreHexColors <- goalie::allAreHexColors
assay <- basejump::assay
calculateMetrics <- basejump::calculateMetrics
import <- basejump::import
isInstalled <- goalie::isInstalled
mpg <- ggplot2::mpg
nonzeroRowsAndCols <- basejump::nonzeroRowsAndCols
rowData <- basejump::rowData
sampleNames <- basejump::sampleNames
## nolint end

rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment
object <- rse

rownames <- head(rownames(rse))
geneIds <- head(as.character(rowData(rse)[["geneId"]]))
geneNames <- head(as.character(rowData(rse)[["geneName"]]))
genes <- geneIds
