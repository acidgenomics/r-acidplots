funs <- list(
    "plotCorrelationHeatmap" = plotCorrelationHeatmap,
    "plotHeatmap" = plotHeatmap,
    "plotQuantileHeatmap" = plotQuantileHeatmap
)

## These plotting functions intentionally error out for datasets containing
## rows or columns containing all zeros.
rse <- nonzeroRowsAndCols(rse)
sce <- nonzeroRowsAndCols(sce)

test_that("SummarizedExperiment", {
    for (fun in funs) {
        object <- rse
        p <- fun(object)
        expect_s3_class(p, "pheatmap")
        ## Plot should contain annotation data.
        expect_true(
            "annotation_legend" %in% p[["gtable"]][["layout"]][["name"]]
        )
        ## Test color and title support.
        ## NOTE This check is failing on R 4.1 due to pheatmap's improper
        ## definition of `brewer.pal` (without `RColorBrewer::`) in formals.
        ## > expect_s3_class(
        ## >     object = fun(
        ## >         object = object,
        ## >         color = NULL,
        ## >         legendColor = NULL,
        ## >         title = NULL
        ## >     ),
        ## >     class = "pheatmap"
        ## > )
        ## Hexadecimal color functions (e.g. viridis).
        expect_s3_class(
            object = fun(
                object = object,
                color = viridis::viridis,
                legendColor = viridis::viridis
            ),
            class = "pheatmap"
        )
        ## Hexadecimal color palettes (e.g. RColorBrewer).
        if (isInstalled("RColorBrewer")) {
            color <- colorRampPalette(
                RColorBrewer::brewer.pal(n = 11L, name = "PuOr")
            )(256L)
            expect_s3_class(
                object = fun(object = object, color = color),
                class = "pheatmap"
            )
        }
        ## Disable interesting groups.
        expect_s3_class(
            object = fun(
                object = object,
                interestingGroups = NULL
            ),
            class = "pheatmap"
        )
    }
})

test_that("SummarizedExperiment : Non-unique samples", {
    for (fun in funs) {
        object <- rse
        assay(object)[, 2L] <- assay(object)[, 1L]
        expect_message(
            object = fun(object),
            regexp = "Non-unique samples detected. Skipping plot."
        )
    }
})

## Note that this is not supported for correlation heatmap.
test_that("SummarizedExperiment : Convert genes to symbols", {
    for (fun in list(
        plotHeatmap,
        plotQuantileHeatmap
    )) {
        object <- rse
        expect_s3_class(
            object = fun(
                object = object,
                convertGenesToSymbols = TRUE,
                showRownames = TRUE
            ),
            class = "pheatmap"
        )
    }
})

test_that("SummarizedExperiment : Invalid pheatmap passthrough", {
    object <- rse
    expect_error(
        object = plotHeatmap(object, show_colnames = FALSE),
        regexp = "camel"
    )
})

test_that("SummarizedExperiment : Row and column scaling", {
    object <- rse
    ## Introduce zero rows, so we can check handling.
    assay(object)[seq_len(2L), ] <- 0L
    expect_true(any(rowSums(assay(object)) == 0L))
    expect_false(any(colSums(assay(object)) == 0L))
    # Error if matrix counts any all zero rows.
    expect_error(
        object = plotHeatmap(object, scale = "row"),
        regexp = "hasNonzeroRowsAndCols"
    )
    # Drop zero rows and now can plot.
    keep <- rowSums(assay(object)) > 0L
    object <- object[keep, ]
    p <- plotHeatmap(
        object = object,
        scale = "row",
        clusterRows = TRUE,
        clusterCols = TRUE
    )
    expect_s3_class(p, "pheatmap")
    p <- plotHeatmap(
        object = object,
        scale = "col",
        clusterRows = TRUE,
        clusterCols = TRUE
    )
    expect_s3_class(p, "pheatmap")
})

test_that("SingleCellExperiment", {
    for (fun in funs) {
        p <- fun(sce)
        expect_s3_class(p, "pheatmap")
    }
})
