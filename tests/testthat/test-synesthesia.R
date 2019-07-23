context("synesthesia palette")

test_that("synesthesia_pal", {
    expect_is(synesthesia_pal, "function")
    expect_is(synesthesia_pal(), "function")
    expect_true(allAreHexColors(synesthesia_pal()(n = 2L)))
})


