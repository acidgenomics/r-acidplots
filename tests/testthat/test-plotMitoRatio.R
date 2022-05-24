## Current example doesn't contain mito genes, so fake this.

test_that("SingleCellExperiment", {
    object <- calculateMetrics(sce)
    colData(object)[["mitoRatio"]] <-
        rnorm(n = ncol(object), mean = 0.1, sd = 0.01)
    p <- plotMitoRatio(object)
    expect_s3_class(p, "ggplot")
})
