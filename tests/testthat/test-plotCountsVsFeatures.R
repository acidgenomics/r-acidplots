context("plotCountsVsFeatures")

test_that("SingleCellExperiment", {
    object <- calculateMetrics(sce)
    p <- plotCountsVsFeatures(object)
    expect_s3_class(p, "ggplot")
})
