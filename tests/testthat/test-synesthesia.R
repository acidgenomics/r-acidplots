context("synesthesia palette")

test_that("synesthesia_pal", {
    expect_is(synesthesia_pal, "function")
    expect_is(synesthesia_pal(), "function")
    expect_true(allAreHexColors(synesthesia_pal()(n = 2L)))
})

with_parameters_test_that(
    "ggplot2 scales", {
        expect_is(f, "function")
        expect_s3_class(f(), "Scale")
    },
    f = list(
        scale_colour_synesthesia_c,
        scale_colour_synesthesia_d,
        scale_fill_synesthesia_c,
        scale_fill_synesthesia_d
    )
)
