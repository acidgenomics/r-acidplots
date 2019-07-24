context("plotZerosVsDepth")

with_parameters_test_that(
    "plotZerosVsDepth", {
        p <- plotZerosVsDepth(object)
        expect_s3_class(p, "ggplot")
    },
    object = list(
        SummarizedExperiment = rse,
        SingleCellExperiment = sce
    )
)
