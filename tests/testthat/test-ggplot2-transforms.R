test_that("acid_coord_flip", {
    p <- ggplot(data = mpg, aes(x = class)) +
        geom_bar()
    ## Note that this doesn't currently support `+` chain.
    expect_type(acid_coord_flip, "closure")
    p <- acid_coord_flip(p)
    expect_s3_class(p, "ggplot")
})
