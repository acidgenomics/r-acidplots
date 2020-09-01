context("plotUpset")

test_that("list", {
    list <- list(
        a = c("a", "b", "c", "d", "e", "f"),
        b = c("b", "c", "d", "e", "f", "g"),
        c = c("c", "d", "e", "f", "g", "h"),
        d = c("d", "e", "f", "g", "h", "i")
    )
    p <- plotUpset(
        object = list,
        nIntersects = Inf,
        orderBySize = FALSE
    )
    expect_s3_class(p, "upset")
})

test_that("data.frame", {
    file <- system.file("extdata", "movies.csv", package = "acidplots")
    data <- import(file)
    p <- plotUpset(data)
    expect_s3_class(p, "upset")
    expect_error(
        object = plotUpset(
            object = data.frame(
                a = c("a", "b"),
                b = c("c", "d")
            )
        ),
        regexp = "Data frame does not contain any columns"
    )
})
