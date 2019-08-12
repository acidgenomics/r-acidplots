context("plotNovelty")

test_that("SingleCellExperiment", {
    object <- calculateMetrics(sce)
    p <- plotNovelty(object)
    expect_s3_class(p, "ggplot")
})
