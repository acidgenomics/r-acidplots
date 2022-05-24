test_that("Homo sapiens", {
    object <- SingleCellExperiment_Seurat
    markers <- KnownMarkers
    invisible(capture.output({
        list <- plotCellTypesPerCluster(
            object = object,
            markers = markers
        )
    }))
    expect_type(list, "list")
    expect_s3_class(list[[1L]][[1L]], "ggplot")
})
