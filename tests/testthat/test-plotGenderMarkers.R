test_that("SummarizedExperiment", {
    object <- rse
    markers <- readRDS(system.file(
        "extdata", "gender-markers.rds",
        package = "AcidPlots"
    ))
    gr <- markers[[camelCase(organism(object))]]
    expect_s4_class(gr, "GenomicRanges")
    expect_true(isSubset(x = c("geneId", "geneName"), y = names(mcols(gr))))
    geneIds <- mcols(gr)[["geneId"]]
    geneNames <- mcols(gr)[["geneName"]]
    seq <- seq_len(length(gr))
    rownames(object)[seq] <- geneIds
    rowData(object)[["geneId"]] <-
        as.character(rowData(object)[["geneId"]])
    rowData(object)[["geneId"]][seq] <- geneIds
    rowData(object)[["geneName"]] <-
        as.character(rowData(object)[["geneName"]])
    rowData(object)[["geneName"]][seq] <- geneNames
    p <- plotGenderMarkers(object)
    expect_s3_class(p, "ggplot")
    expect_true(isSubset(
        x = as.character(geneNames),
        y = p[["data"]][["rowname"]]
    ))
})
