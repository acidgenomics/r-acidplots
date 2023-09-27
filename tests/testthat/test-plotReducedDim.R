object <- SingleCellExperiment_Seurat

test_that("SCE", {
    p <- plotReducedDim(
        object = object,
        pointsAsNumbers = TRUE,
        dark = TRUE,
        label = FALSE
    )
    expect_s3_class(p, "ggplot")
})

test_that("Aliases", {
    for (fun in list(plotPca, plotTsne, plotUmap)) {
        p <- fun(object)
        expect_s3_class(p, "ggplot")
    }
})
