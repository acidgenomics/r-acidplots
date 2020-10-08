#' @name plotFeaturesPerCell
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotFeaturesPerCell
#' @note Updated 2019-09-16.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- calculateMetrics(object)
#' plotFeaturesPerCell(object)
NULL



## Updated 2019-09-16.
`plotFeaturesPerCell,SingleCellExperiment` <-  # nolint
    function(
        object,
        geom,
        interestingGroups = NULL,
        min = 0L,
        max = Inf,
        trans = "log2",
        fill,
        labels
    ) {
        do.call(
            what = .plotQCMetric,
            args = list(
                object = object,
                metricCol = "nFeature",
                geom = match.arg(geom),
                interestingGroups = interestingGroups,
                min = min,
                max = max,
                trans = trans,
                fill = fill,
                labels = matchLabels(
                    labels = labels,
                    choices = eval(formals()[["labels"]])
                )
            )
        )
    }

f <- formals(`plotFeaturesPerCell,SingleCellExperiment`)
f[["fill"]] <- formalsList[["fill.discrete"]]
f[["geom"]] <- .geom
f[["labels"]] <- formals(.plotQCMetric)[["labels"]]
f[["labels"]][["title"]] <- "Features per cell"
f[["labels"]][["metricAxis"]] <- "features"
formals(`plotFeaturesPerCell,SingleCellExperiment`) <- f



#' @rdname plotFeaturesPerCell
#' @export
setMethod(
    f = "plotFeaturesPerCell",
    signature = signature("SingleCellExperiment"),
    definition = `plotFeaturesPerCell,SingleCellExperiment`
)
