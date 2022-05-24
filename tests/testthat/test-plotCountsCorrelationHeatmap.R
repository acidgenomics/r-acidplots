test_that("matrix", {
    x <- assay(rse)
    y <- x + 1L
    p <- plotCountsCorrelationHeatmap(x, y)
    expect_s3_class(p, "pheatmap")
})
