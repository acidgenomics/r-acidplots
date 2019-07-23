context("geoms")

test_that("acid_geom_abline", {
    ## x-axis line
    geom <- acid_geom_abline(xintercept = 1L)
    expect_s3_class(geom, "ggproto")

    ## y-axis line
    geom <- acid_geom_abline(yintercept = 1L)
    expect_s3_class(geom, "ggproto")
})

test_that("acid_geom_label", {
    geom <- acid_geom_label()
    expect_s3_class(geom, "ggproto")
})

test_that("acid_geom_label_average", {
    data = tibble(
        sampleName = rep(c("sample1", "sample2"), times = 4L),
        counts = seq_len(8L)
    )
    geom <- acid_geom_label_average(
        data = data,
        col = "counts",
        fun = "mean"
    )
    expect_s3_class(geom, "ggproto")
})

test_that("acid_geom_label_repel", {
    geom <- acid_geom_label_repel()
    expect_s3_class(geom, "ggproto")
})
