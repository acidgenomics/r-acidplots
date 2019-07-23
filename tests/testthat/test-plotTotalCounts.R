context("plotTotalCounts")

with_parameters_test_that(
    "plotTotalCounts", {
        p <- plotTotalCounts(object)
        expect_s3_class(p, "ggplot")
    },
    object = list(
        SummarizedExperiment = rse,
        SingleCellExperiment = sce
    )
)
