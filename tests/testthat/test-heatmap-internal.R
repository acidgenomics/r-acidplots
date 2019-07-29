context("heatmap internal")

test_that(".hclust", {
    x <- .hclust(matrix_lfc, rows = TRUE, cols = TRUE)
    expect_is(x, "list")
    expect_identical(
        lapply(x, class),
        list(
            rows = "hclust",
            cols = "hclust"
        )
    )
})

test_that(".scaleMatrix", {
    x <- .scaleMatrix(matrix_lfc, scale = "none")
    expect_identical(x, matrix_lfc)

    x <- .scaleMatrix(matrix_lfc, scale = "row")
    expect_true(all(round(rowSums(x)) == 0L))

    x <- .scaleMatrix(matrix_lfc, scale = "col")
    expect_true(all(round(colSums(x)) == 0L))
})
