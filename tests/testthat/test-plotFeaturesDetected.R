context("plotFeaturesDetected")

test_that("plotFeaturesDetected", {
    for (object in list(
        SummarizedExperiment = rse,
        SingleCellExperiment = sce
    )) {
        p <- plotFeaturesDetected(object)
        expect_s3_class(p, "ggplot")
    }
})
