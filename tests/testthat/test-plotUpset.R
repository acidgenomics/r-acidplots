test_that("list", {
    list <- list(
        "aaa" = c("a", "b", "c", "d", "e", "f"),
        "bbb" = c("b", "c", "d", "e", "f", "g"),
        "ccc" = c("c", "d", "e", "f", "g", "h"),
        "ddd" = c("d", "e", "f", "g", "h", "i")
    )
    p <- plotUpset(
        object = list,
        nIntersections = Inf,
        orderBySize = FALSE
    )
    expect_s3_class(p, "patchwork")
})

test_that("data.frame", {
    file <- system.file("extdata", "movies.csv", package = "AcidPlots")
    data <- import(file)
    p <- plotUpset(data)
    expect_s3_class(p, "patchwork")
    expect_error(
        object = plotUpset(
            object = data.frame(
                "aaa" = c("a", "b"),
                "bbb" = c("c", "d")
            )
        ),
        regexp = "0, 1"
    )
})
