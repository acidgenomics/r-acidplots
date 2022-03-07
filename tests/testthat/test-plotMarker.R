context("plotMarker")

test_that("SCE", {
    object <- SingleCellExperiment_Seurat
    genes <- head(rownames(object))
    for (expression in eval(formals(`plotMarker,SCE`)[["expression"]])) {
        p <- plotMarker(
            object = object,
            genes = genes,
            expression = expression
        )
        expect_s3_class(p, "ggplot")
    }
})
