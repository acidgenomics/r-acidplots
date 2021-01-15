context("plotGenderMarkers")

test_that("SummarizedExperiment", {
    object <- rse
    data(genderMarkers, envir = environment())
    organism <- "Homo sapiens"
    expect_identical(organism(object), organism)
    markers <- genderMarkers[[camelCase(organism(object))]]
    geneIds <- markers[["geneId"]]
    geneNames <- markers[["geneName"]]
    seq <- seq_len(nrow(markers))
    rownames(object)[seq] <- geneIds
    rowData(object)[["geneId"]] <- as.character(rowData(object)[["geneId"]])
    rowData(object)[["geneId"]][seq] <- geneIds
    rowData(object)[["geneName"]] <-
        as.character(rowData(object)[["geneName"]])
    rowData(object)[["geneName"]][seq] <- geneNames
    p <- plotGenderMarkers(object)
    expect_s3_class(p, "ggplot")
    expect_true(all(geneNames %in% p[["data"]][["rowname"]]))
})
