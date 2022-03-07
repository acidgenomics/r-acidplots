## Fix for pheatmap partial match warning.
## https://github.com/raivokolde/pheatmap/issues/46
options(
    warnPartialMatchAttr = FALSE,
    warnPartialMatchDollar = FALSE
)

data(
    RangedSummarizedExperiment,
    SingleCellExperiment_splatter,
    matrix,
    matrix_lfc,
    package = "AcidTest",
    envir = environment()
)

## nolint start
`assay<-` <- SummarizedExperiment::`assay<-`
`colData<-` <- SummarizedExperiment::`colData<-`
`rowData<-` <- SummarizedExperiment::`rowData<-`
allAreHexColors <- goalie::allAreHexColors
assay <- SummarizedExperiment::assay
calculateMetrics <- AcidGenerics::calculateMetrics
import <- pipette::import
isInstalled <- goalie::isInstalled
mpg <- ggplot2::mpg
nonzeroRowsAndCols <- AcidGenerics::nonzeroRowsAndCols
rowData <- SummarizedExperiment::rowData
sampleNames <- Biobase::sampleNames
## nolint end

rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment_splatter
object <- rse

rownames <- head(rownames(rse))
geneIds <- head(as.character(rowData(rse)[["geneId"]]))
geneNames <- head(as.character(rowData(rse)[["geneName"]]))
genes <- geneIds
