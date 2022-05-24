test_that("plotCorrelationHeatmap", {
    for (object in list(
        SummarizedExperiment = rse,
        SingleCellExperiment = sce
    )) {
        x <- plotCorrelationHeatmap(object)
        expect_s3_class(x, "pheatmap")
    }
})
