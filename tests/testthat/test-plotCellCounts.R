test_that("SingleCellExperiment", {
    p <- plotCellCounts(sce)
    expect_s3_class(p, "ggplot")
})
