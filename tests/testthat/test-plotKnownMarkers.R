context("plotKnownMarkers")

test_that("SCE", {
    object <- SingleCellExperiment_Seurat
    markers <- head(KnownMarkers, n = 2L)
    invisible(capture.output({
        x <- plotKnownMarkers(
            object = object,
            markers = markers
        )
    }))
    expect_type(x, "list")
    expect_s3_class(x[[1L]][[1L]], "ggplot")
})
