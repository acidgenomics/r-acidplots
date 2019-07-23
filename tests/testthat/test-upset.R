context("upset")

test_that("upset", {
    movies <- read.csv(
        system.file("extdata", "movies.csv", package = "UpSetR"),
        header = TRUE,
        sep = ";"
    )
    p <- upset(movies)
    expect_s3_class(p, "upset")
})
