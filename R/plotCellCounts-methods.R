#' @name plotCellCounts
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotCellCounts
#' @note Updated 2022-05-06.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' plotCellCounts(object)
NULL



## Updated 2022-05-06.
`plotCellCounts,SCE` <- # nolint
    function(object,
             assay = 1L,
             interestingGroups = NULL,
             labels = list(
                 "title" = "Cell counts",
                 "subtitle" = NULL,
                 "x" = NULL,
                 "y" = "cells"
             )) {
        validObject(object)
        assert(hasMultipleSamples(object))
        labels <- matchLabels(labels)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        if (!hasMetrics(object)) {
            suppressMessages({
                object <- calculateMetrics(object, assay = assay)
            })
        }
        idCol <- matchSampleColumn(object)
        colData <- colData(object)
        assert(isSubset(idCol, colnames(colData)))
        metric <- table(colData[[idCol]])
        sampleData <- sampleData(object)
        assert(areSetEqual(names(metric), rownames(sampleData)))
        data <- sampleData
        metricCol <- "nCells"
        data[[metricCol]] <- as.integer(metric[rownames(data)])
        ## Plot.
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = !!sym("sampleName"),
                y = !!sym(metricCol),
                fill = str_replace_na(!!sym("interestingGroups"))
            )
        ) +
            acid_geom_bar() +
            acid_scale_y_continuous_nopad()
        ## Labels.
        labels[["fill"]] <- paste(interestingGroups, collapse = ":\n")
        p <- p + do.call(what = labs, args = labels)
        ## Color palette.
        p <- p + autoDiscreteFillScale()
        ## Labels.
        if (isTRUE(nrow(data) <= 16L)) {
            p <- p + acid_geom_label(
                data = as.data.frame(data),
                mapping = aes(label = !!sym(metricCol)),
                ## Align the label just under the top of the bar.
                vjust = 1.25
            )
        }
        ## Facets.
        facets <- NULL
        if (isSubset("aggregate", colnames(data))) {
            facets <- c(facets, "aggregate")
        }
        if (is.character(facets)) {
            p <- p + facet_wrap(facets = !!syms(facets), scales = "free")
        }
        ## Return.
        p
    }


#' @rdname plotCellCounts
#' @export
setMethod(
    f = "plotCellCounts",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotCellCounts,SCE`
)
