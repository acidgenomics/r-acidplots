test_that("acid_pretty_breaks", {
    x <- acid_pretty_breaks()
    expect_type(x, "closure")
})
