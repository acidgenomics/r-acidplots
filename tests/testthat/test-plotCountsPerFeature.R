context("plotCountsPerFeature")

with_parameters_test_that(
    "RSE, SCE", {
        p <- plotCountsPerFeature(object)
        expect_s3_class(p, "ggplot")
    },
    object = list(rse, sce)
)

geom <- eval(formals(`plotCountsPerFeature,SummarizedExperiment`)[["geom"]])
with_parameters_test_that(
    "geom", {
        p <- plotCountsPerFeature(rse, geom = geom)
        expect_s3_class(p, "ggplot")
    },
    geom = geom
)
