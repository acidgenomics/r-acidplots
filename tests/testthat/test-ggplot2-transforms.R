test_that("acid_discrete_coord_flip", {
    p <- ggplot(data = mpg, aes(x = class)) +
        geom_bar() +
        acid_discrete_coord_flip()
    expect_s3_class(p, "ggplot")
})
