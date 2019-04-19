context("plotCounts")

#' data(rse, package = "acidtest")
#'
#' rownames <- head(rownames(rse))
#' print(rownames)
#' g2s <- basejump::Gene2Symbol(rse)
#' geneIDs <- head(g2s[["geneID"]])
#' print(geneIDs)
#' geneNames <- head(g2s[["geneName"]])
#' print(geneNames)
#'
#' ## Rownames, gene IDs, and gene names (symbols) are supported.
#' plotCounts(rse, genes = geneIDs, style = "facet")
#' plotCounts(rse, genes = geneNames, style = "wide")

test_that("SummarizedExperiment", {
    x <- plotCounts(rse, genes = genes)
})
