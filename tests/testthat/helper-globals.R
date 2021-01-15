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

rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment

rownames <- head(rownames(rse))
g2s <- basejump::Gene2Symbol(rse)
geneIds <- head(g2s[[1L]])
geneNames <- head(g2s[[2L]])

object <- rse
genes <- geneIds

mpg <- ggplot2::mpg

## nolint start
allAreHexColors <- goalie::allAreHexColors
assay <- SummarizedExperiment::assay
`assay<-` <- SummarizedExperiment::`assay<-`
`rowData<-` <- SummarizedExperiment::`rowData<-`
## nolint end
