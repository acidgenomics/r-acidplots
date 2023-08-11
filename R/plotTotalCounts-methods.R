#' @name plotTotalCounts
#' @inherit AcidGenerics::plotTotalCounts
#' @note Updated 2022-03-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment_splatter,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotTotalCounts(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' plotTotalCounts(object)
NULL



## Updated 2021-09-10.
`plotTotalCounts,SE` <- # nolint
    function(object,
             assay = 1L,
             interestingGroups = NULL,
             perMillion = FALSE,
             labels = list(
                 "title" = "Total counts",
                 "subtitle" = NULL,
                 "x" = NULL,
                 "y" = "counts"
             ),
             flip) {
        validObject(object)
        assert(
            isScalar(assay),
            isFlag(perMillion),
            isFlag(flip)
        )
        labels <- matchLabels(labels)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        metricCol <- "totalCounts"
        counts <- assay(object, i = assay)
        data <- sampleData(object)
        if (is(counts, "Matrix")) {
            assert(requireNamespaces("Matrix"))
            colSums <- Matrix::colSums
        }
        data[[metricCol]] <- colSums(counts)
        if (isTRUE(perMillion)) {
            data[[metricCol]] <- data[[metricCol]] / 1e6L
            labels[["y"]] <- paste(labels[["y"]], "(per million)")
        }
        ## Plot.
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[["sampleName"]],
                y = .data[[metricCol]],
                fill = str_replace_na(.data[["interestingGroups"]])
            )
        ) +
            acid_geom_bar() +
            acid_scale_y_continuous_nopad()
        ## Labels.
        labels[["fill"]] <- paste(interestingGroups, collapse = ":\n")
        p <- p + do.call(what = labs, args = labels)
        ## Color palette.
        p <- p + acid_scale_fill_discrete()
        ## Flip.
        if (isTRUE(flip)) {
            p <- p + acid_discrete_coord_flip()
        }
        ## Hide sample name legend.
        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(fill = "none")
        }
        ## Return.
        p
    }

formals(`plotTotalCounts,SE`)[["flip"]] <- # nolint
    .formalsList[["flip"]]



## Updated 2019-09-15.
`plotTotalCounts,SCE` <- # nolint
    function(object, ...) {
        plotTotalCounts(
            object = aggregateCellsToSamples(object),
            ...
        )
    }



#' @describeIn plotTotalCounts Applies `aggregateCellsToSamples()` calculation
#' to summarize at sample level prior to plotting.\cr
#' Passes `...` to `SummarizedExperiment` method.
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotTotalCounts,SCE`
)

#' @rdname plotTotalCounts
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotTotalCounts,SE`
)
