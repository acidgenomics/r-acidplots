context("synesthesia palette")

test_that("synesthesia_pal", {
    expect_is(synesthesia_pal, "function")
    expect_is(synesthesia_pal(), "function")
    expect_true(allAreHexColors(synesthesia_pal()(n = 2L)))
})

test_that("ggplot2 scales", {
    for (f in list(
        scale_color_synesthesia_c,
        scale_color_synesthesia_d,
        scale_fill_synesthesia_c,
        scale_fill_synesthesia_d
    )) {
        expect_is(f, "function")
        expect_s3_class(f(), "Scale")
    }
})
