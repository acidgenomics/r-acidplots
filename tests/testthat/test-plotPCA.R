test_that("SummarizedExperiment", {
    p <- plotPCA(rse, label = FALSE)
    expect_s3_class(p, "ggplot")
    p <- plotPCA(rse, label = TRUE)
    expect_s3_class(p, "ggplot")
    ## Check that data frame saved inside ggplot object has attributes.
    expect_is(attr(p[["data"]], "percentVar"), "numeric")
})
