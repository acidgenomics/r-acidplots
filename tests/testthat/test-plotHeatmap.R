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

test_that("Invalid pheatmap passthrough", {
    expect_error(
        object = plotHeatmap(object, show_colnames = FALSE),
        regexp = "Specify arguments in camel case: show_colnames"
    )
})
