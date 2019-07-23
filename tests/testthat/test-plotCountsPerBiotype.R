context("plotCountsPerBiotype")

test_that("SummarizedExperiment", {
    x <- plotCountsPerBiotype(rse)
    expect_s3_class(x, "ggplot")
})



context("plotCountsPerBroadClass")

test_that("SummarizedExperiment", {
    x <- plotCountsPerBroadClass(rse)
    expect_s3_class(x, "ggplot")
})
