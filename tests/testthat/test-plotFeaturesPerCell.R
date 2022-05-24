test_that("SingleCellExperiment", {
    object <- calculateMetrics(sce)
    p <- plotFeaturesPerCell(object)
    expect_s3_class(p, "ggplot")
})
