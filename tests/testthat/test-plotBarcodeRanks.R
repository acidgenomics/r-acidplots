context("plotBarcodeRanks")

skip_if_not(packageVersion("DropletUtils") >= "1.4")

test_that("SingleCellExperiment", {
    p <- plotBarcodeRanks(sce)
    expect_s3_class(p, "ggplot")
})
