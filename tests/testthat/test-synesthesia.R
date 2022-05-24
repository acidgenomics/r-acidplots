test_that("synesthesia_pal", {
    expect_type(synesthesia_pal, "closure")
    expect_type(synesthesia_pal(), "closure")
    expect_true(allAreHexColors(synesthesia_pal()(n = 2L)))
})

test_that("ggplot2 scales", {
    for (f in list(
        scale_color_synesthesia_c,
        scale_color_synesthesia_d,
        scale_fill_synesthesia_c,
        scale_fill_synesthesia_d
    )) {
        expect_type(f, "closure")
        expect_s3_class(f(), "Scale")
    }
})
