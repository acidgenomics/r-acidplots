test_that("matrix", {
    x <- assay(rse)
    x <- x[seq_len(4L), seq_len(2L)]
    y <- x * 2L
    p <- plotCountsCorrelation(x, y)
    expect_s3_class(p, "ggplot")
})
