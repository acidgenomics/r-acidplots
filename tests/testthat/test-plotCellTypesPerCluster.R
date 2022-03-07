context("plotCellTypesPerCluster")

test_that("Homo sapiens", {
    data(
        KnownMarkers,
        SingleCellExperiment_Seurat,
        package = "AcidTest",
        envir = environment()
    )
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
