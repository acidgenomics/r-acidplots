data(
    RangedSummarizedExperiment,
    SingleCellExperiment,
    package = "acidtest",
    envir = environment()
)

rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment

rownames <- head(rownames(rse))
g2s <- basejump::Gene2Symbol(rse)
geneIDs <- head(g2s[["geneID"]])
geneNames <- head(g2s[["geneName"]])

object <- rse
genes <- geneIDs

mpg <- ggplot2::mpg

allAreHexColors <- goalie::allAreHexColors
assay <- SummarizedExperiment::assay
tibble <- tibble::tibble
