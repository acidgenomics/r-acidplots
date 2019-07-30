context("plotHeatmap")

funs <- list(
    plotCorrelationHeatmap,
    plotHeatmap,
    plotQuantileHeatmap
)

with_parameters_test_that(
    "SummarizedExperiment", {
        p <- fun(object)
        expect_is(p, "pheatmap")
        ## Plot should contain annotation data.
        expect_true(
            "annotation_legend" %in% p[["gtable"]][["layout"]][["name"]]
        )

        ## Test color and title support.
        expect_is(
            object = fun(
                object = object,
                color = NULL,
                legendColor = NULL,
                title = NULL
            ),
            class = "pheatmap"
        )

        ## Hexadecimal color functions (e.g. viridis).
        expect_is(
            object = fun(
                object = object,
                color = viridis::viridis,
                legendColor = viridis::viridis
            ),
            class = "pheatmap"
        )

        ## Hexadecimal color palettes (e.g. RColorBrewer).
        color <- colorRampPalette(
            RColorBrewer::brewer.pal(n = 11L, name = "PuOr")
        )(256L)
        expect_is(
            object = fun(object = object, color = color),
            class = "pheatmap"
        )

        ## Disable interesting groups.
        expect_is(
            object = fun(
                object = object,
                interestingGroups = NULL
            ),
            class = "pheatmap"
        )
    },
    fun = funs
)

with_parameters_test_that(
    "SingleCellExperiment", {
        p <- fun(object)
        expect_s3_class(p, "pheatmap")
    },
    fun = funs
)

with_parameters_test_that(
    "Non-unique samples", {
        assay(rse)[, 2L] <- assay(rse)[, 1L]
        expect_warning(
            object = fun(rse),
            regexp = "Non-unique samples detected. Skipping plot."
        )
    },
    fun = funs
)

test_that("Invalid pheatmap passthrough", {
    expect_error(
        object = plotHeatmap(object, show_colnames = FALSE),
        regexp = "Specify arguments in camel case: show_colnames"
    )
})

test_that("Row and column scaling", {
    object <- rse

    ## Introduce zero rows, so we can check handling.
    assay(object)[seq_len(2L), ] <- 0L


    expect_true(any(rowSums(assay(object)) == 0L))
    expect_false(any(colSums(assay(object)) == 0L))

    # Error if matrix counts any all zero rows.
    expect_error(
        object = plotHeatmap(object, scale = "row"),
        regexp = "hasNonZeroRowsAndCols"
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
