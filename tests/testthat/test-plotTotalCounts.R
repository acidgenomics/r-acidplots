test_that("plotTotalCounts", {
    for (object in list(
        SummarizedExperiment = rse,
        SingleCellExperiment = sce
    )) {
        p <- plotTotalCounts(object)
        expect_s3_class(p, "ggplot")
    }
})
