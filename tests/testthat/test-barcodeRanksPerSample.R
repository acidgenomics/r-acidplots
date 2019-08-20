context("barcodeRanksPerSample")

skip_if_not_installed(pkg = "DropletUtils", minimum_version = "1.4")

test_that("SingleCellExperiment", {
    x <- barcodeRanksPerSample(sce)
    expect_s4_class(x, "DataFrameList")
})
