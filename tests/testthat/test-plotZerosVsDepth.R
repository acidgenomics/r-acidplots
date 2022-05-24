test_that("plotZerosVsDepth", {
    for (object in list(
        SummarizedExperiment = rse,
        SingleCellExperiment = sce
    )) {
        p <- plotZerosVsDepth(object)
        expect_s3_class(p, "ggplot")
    }
})
