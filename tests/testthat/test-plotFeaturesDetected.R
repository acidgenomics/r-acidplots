context("plotFeaturesDetected")

with_parameters_test_that(
    "plotFeaturesDetected", {
        p <- plotFeaturesDetected(object)
        expect_s3_class(p, "ggplot")
    },
    object = list(
        SummarizedExperiment = rse,
        SingleCellExperiment = sce
    )
)
