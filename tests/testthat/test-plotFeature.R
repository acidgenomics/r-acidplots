context("plotFeature")

test_that("SCE", {
    object <- SingleCellExperiment_Seurat
    features <- c("nCount_RNA", "nFeature_RNA")
    p <- plotFeature(object, features = features)
    expect_s3_class(p, "ggplot")
})
