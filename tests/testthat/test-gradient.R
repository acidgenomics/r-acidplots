context("gradient functions")

with_parameters_test_that(
    "gradient functions", {
        x <- f(n = 3L)
        expect_true(allAreHexColors(x))
    },
    f = list(
        blueYellow,
        purpleOrange
    )
)
