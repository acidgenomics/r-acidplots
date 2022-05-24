test_that("SingleCellExperiment", {
    object <- calculateMetrics(sce)
    p <- plotCountsVsFeatures(object, trendline = TRUE)
    expect_s3_class(p, "ggplot")
})
