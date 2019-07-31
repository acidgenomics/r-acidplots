context("plotGenderMarkers")

test_that("SummarizedExperiment", {
    object <- rse
    data(genderMarkers, envir = environment())
    organism <- "Homo sapiens"
    expect_identical(organism(object), organism)
    markers <- genderMarkers[[camel(organism(object))]]
    geneIDs <- markers[["geneID"]]
    geneNames <- markers[["geneName"]]
    seq <- seq_len(nrow(markers))
    rownames(object)[seq] <- geneIDs
    rowData(object)[["geneID"]] <- as.character(rowData(object)[["geneID"]])
    rowData(object)[["geneID"]][seq] <- geneIDs
    rowData(object)[["geneName"]] <-
        as.character(rowData(object)[["geneName"]])
    rowData(object)[["geneName"]][seq] <- geneNames
    p <- plotGenderMarkers(object)
    expect_s3_class(p, "ggplot")
    expect_true(all(geneNames %in% p[["data"]][["rowname"]]))
})
