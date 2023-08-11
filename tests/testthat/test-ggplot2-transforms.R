test_that("acid_discrete_coord_flip", {
    p <- ggplot(data = mpg, aes(x = class)) +
        geom_bar() +
        acid_discrete_coord_flip()
    expect_s3_class(p, "ggplot")
    pdata <- ggplot_build(p)[["data"]][[1L]]
    expect_identical(
        object = as.integer(pdata[["x"]]),
        expected = rev(seq(from = 1L, to = nrow(pdata), by = 1L))
    )
})
