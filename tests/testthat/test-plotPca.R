test_that("SummarizedExperiment", {
    p <- plotPca(rse, label = FALSE)
    expect_s3_class(p, "ggplot")
    p <- plotPca(rse, label = TRUE)
    expect_s3_class(p, "ggplot")
    ## Check that data frame saved inside ggplot object has attributes.
    expect_type(attr(p[["data"]], "percentVar"), "double")
})
