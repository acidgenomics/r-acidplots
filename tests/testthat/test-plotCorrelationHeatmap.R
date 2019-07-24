context("plotCorrelationHeatmap")

with_parameters_test_that(
    "plotCorrelationHeatmap", {
        x <- plotCorrelationHeatmap(object)
        expect_s3_class(x, "pheatmap")
    },
    object = list(
        SummarizedExperiment = rse,
        SingleCellExperiment = sce
    )
)
