context("plotQC")

test_that("SummarizedExperiment", {
    p <- plotQC(rse)
    expect_s3_class(p, "ggplot")
})
