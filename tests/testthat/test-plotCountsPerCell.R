context("plotCountsPerCell")

test_that("SingleCellExperiment", {
    object <- calculateMetrics(sce)
    p <- plotCountsPerCell(object)
    expect_s3_class(p, "ggplot")
})
