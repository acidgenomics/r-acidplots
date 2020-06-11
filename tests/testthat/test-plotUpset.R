context("plotUpset")

test_that("data.frame", {
    file <- system.file("extdata", "movies.csv", package = "acidplots")
    data <- import(file)
    p <- plotUpset(data)
    expect_s3_class(p, "upset")
})
