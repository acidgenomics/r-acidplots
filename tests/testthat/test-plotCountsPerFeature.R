context("plotCountsPerFeature")

test_that("SummarizedExperiment", {
    p <- plotCountsPerFeature(rse, geom = "boxplot")
    expect_s3_class(p, "ggplot")

    p <- plotCountsPerFeature(rse, geom = "density")
    expect_s3_class(p, "ggplot")
})

test_that("SingleCellExperiment", {
    p <- plotCountsPerFeature(sce)
    expect_s3_class(p, "ggplot")
})
