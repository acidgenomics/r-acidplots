#' @name plotFeaturesPerCell
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::plotFeaturesPerCell
#' @note Updated 2019-08-12.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- calculateMetrics(object)
#' plotFeaturesPerCell(object)
NULL



#' @rdname plotFeaturesPerCell
#' @name plotFeaturesPerCell
#' @importFrom bioverbs plotFeaturesPerCell
#' @usage plotFeaturesPerCell(object, ...)
#' @export
NULL



## Updated 2019-08-12.
`plotFeaturesPerCell,SingleCellExperiment` <-  # nolint
    function(
        object,
        geom,
        interestingGroups = NULL,
        min = 0L,
        max = Inf,
        trans = "log2",
        fill,
        title = "Features per cell"
    ) {
        geom <- match.arg(geom)
        do.call(
            what = .plotQCMetric,
            args = list(
                object = object,
                metricCol = "nFeature",
                geom = geom,
                interestingGroups = interestingGroups,
                min = min,
                max = max,
                trans = trans,
                fill = fill,
                title = title
            )
        )
    }

formals(`plotFeaturesPerCell,SingleCellExperiment`)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(`plotFeaturesPerCell,SingleCellExperiment`)[["geom"]] <- geom



#' @rdname plotFeaturesPerCell
#' @export
setMethod(
    f = "plotFeaturesPerCell",
    signature = signature("SingleCellExperiment"),
    definition = `plotFeaturesPerCell,SingleCellExperiment`
)
