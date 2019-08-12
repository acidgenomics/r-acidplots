context("plotBarcodeRanks")

test_that("SingleCellExperiment", {
    p <- plotBarcodeRanks(sce)
    expect_s3_class(p, "ggplot")
})
