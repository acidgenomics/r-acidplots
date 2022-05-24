skip_if_not_installed(pkg = "DropletUtils", minimum_version = "1.4")

test_that("SingleCellExperiment", {
    p <- plotBarcodeRanks(sce)
    expect_s3_class(p, "ggplot")
})
