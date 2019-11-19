context("plotUpset")

test_that("data.frame", {
    movies <- import(system.file("extdata", "movies.csv", package = "UpSetR"))
    p <- plotUpset(movies)
    expect_s3_class(p, "upset")
})
