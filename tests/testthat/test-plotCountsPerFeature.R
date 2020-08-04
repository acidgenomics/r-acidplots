context("plotCountsPerFeature")

test_that("RSE, SCE", {
    for (object in list(rse, sce)) {
        p <- plotCountsPerFeature(object)
        expect_s3_class(p, "ggplot")
    }
})

geoms <- eval(formals(`plotCountsPerFeature,SummarizedExperiment`)[["geom"]])
test_that("geom", {
    for (geom in geoms) {
        p <- plotCountsPerFeature(rse, geom = geom)
        expect_s3_class(p, "ggplot")
    }
})
