context("barcodeRanksPerSample")

test_that("SingleCellExperiment", {
    x <- barcodeRanksPerSample(sce)
    expect_s4_class(x, "DataFrameList")
})
