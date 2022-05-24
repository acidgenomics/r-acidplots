## Current example doesn't contain mito genes, so fake this.

test_that("SingleCellExperiment", {
    object <- calculateMetrics(sce)
    colData(object)[["nMito"]] <- colData(object)[["nCoding"]] * 0.1
    p <- plotMitoVsCoding(object)
    expect_s3_class(p, "ggplot")
})
