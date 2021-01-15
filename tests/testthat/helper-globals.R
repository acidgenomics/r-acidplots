## Fix for pheatmap partial match warning.
## https://github.com/raivokolde/pheatmap/issues/46
options(
    warnPartialMatchAttr = FALSE,
    warnPartialMatchDollar = FALSE
)

## nolint start
allAreHexColors <- goalie::allAreHexColors
assay <- SummarizedExperiment::assay
`assay<-` <- SummarizedExperiment::`assay<-`
`rowData<-` <- SummarizedExperiment::`rowData<-`
## nolint end

data(
    RangedSummarizedExperiment,
    SingleCellExperiment,
    matrix,
    matrix_lfc,
    package = "AcidTest",
    envir = environment()
)

rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment
object <- rse

rownames <- head(rownames(rse))
geneIds <- head(as.character(rowData(rse)[["geneId"]]))
geneNames <- head(as.character(rowData(rse)[["geneName"]]))
genes <- geneIds

mpg <- ggplot2::mpg
