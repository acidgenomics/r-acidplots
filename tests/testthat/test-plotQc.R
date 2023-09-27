test_that("grid", {
    p <- plotQc(rse)
    expect_s3_class(p, "ggplot")
})



sce <- calculateMetrics(sce)

test_that("grid", {
    p <- plotQc(sce)
    expect_s3_class(p, "ggplot")
})

geoms <- eval(formals(`plotQc,SCE`)[["geom"]])
test_that("geom", {
    for (geom in geoms) {
        p <- plotQc(sce, geom = geom)
        expect_s3_class(p, "ggplot")
    }
})
