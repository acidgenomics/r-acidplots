#' @name plotFeaturesDetected
#' @inherit AcidGenerics::plotFeaturesDetected
#' @note Updated 2021-09-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotFeaturesDetected(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotFeaturesDetected(object)
NULL



## Updated 2021-09-10.
`plotFeaturesDetected,SE` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        limit = 0L,
        minCounts = 1L,
        labels = list(
            "title" = "Features detected",
            "subtitle" = NULL,
            "x" = NULL,
            "y" = "features"
        ),
        flip
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isInt(limit) && isNonNegative(limit),
            isInt(minCounts) && isNonNegative(minCounts),
            isFlag(flip)
        )
        labels <- matchLabels(labels)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        counts <- assay(object, i = assay)
        featureCount <- colSums(counts >= minCounts)
        data <- metrics(object)
        data[["featureCount"]] <- featureCount
        ## Plot.
        data <- as_tibble(data, rownames = NULL)
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("sampleName"),
                y = !!sym("featureCount"),
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
        ## Show limit line.
        if (isPositive(limit)) {
            p <- p + acid_geom_abline(yintercept = limit)
        }
        ## Flip.
        if (isTRUE(flip)) {
            p <- acid_coord_flip(p)
        }
        ## Hide sample name legend.
        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(fill = "none")
        }
        ## Return.
        p
    }

formals(`plotFeaturesDetected,SE`)[["flip"]] <- formalsList[["flip"]]



## Updated 2019-09-15.
`plotFeaturesDetected,SCE` <-  # nolint
    function(object, ...) {
        plotFeaturesDetected(
            object = aggregateCellsToSamples(object),
            ...
        )
    }



#' @describeIn plotFeaturesDetected Applies `aggregateCellsToSamples()`
#'   calculation to summarize at sample level prior to plotting.\cr
#'   Passes `...` to `SummarizedExperiment` method.
#' @export
setMethod(
    f = "plotFeaturesDetected",
    signature = signature("SingleCellExperiment"),
    definition = `plotFeaturesDetected,SCE`
)

#' @rdname plotFeaturesDetected
#' @export
setMethod(
    f = "plotFeaturesDetected",
    signature = signature("SummarizedExperiment"),
    definition = `plotFeaturesDetected,SE`
)
