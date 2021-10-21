#' @name plotCellCounts
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotCellCounts
#' @note Updated 2021-09-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotCellCounts(object)
NULL



## Updated 2021-09-10.
`plotCellCounts,SCE` <-  # nolint
    function(
        object,
        interestingGroups = NULL,
        labels = list(
            "title" = "Cell counts",
            "subtitle" = NULL,
            "x" = NULL,
            "y" = "cells"
        )
    ) {
        validObject(object)
        labels <- matchLabels(labels)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        idCol <- matchSampleColumn(object)
        colData <- colData(object)
        assert(isSubset(idCol, colnames(colData)))
        metric <- table(colData[[idCol]])
        sampleData <- sampleData(object)
        assert(identical(names(metric), rownames(sampleData)))
        data <- sampleData
        metricCol <- "nCells"
        data[[metricCol]] <- as.integer(metric)
        ## Plot.
        data <- as_tibble(data, rownames = NULL)
        p <- ggplot(
            data = data,
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
                data = data,
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
            p <- p + facet_wrap(facets = syms(facets), scales = "free")
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
