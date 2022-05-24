test_that("gradient functions", {
    for (f in list(blueYellow, purpleOrange)) {
        x <- f(n = 3L)
        expect_true(allAreHexColors(x))
    }
})
