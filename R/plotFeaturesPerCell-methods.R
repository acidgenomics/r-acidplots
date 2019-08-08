#' @name plotFeaturesPerCell
#' @author Michael Steinbaugh, Rory Kirchner
#' @include globals.R
#' @inherit bioverbs::plotFeaturesPerCell
#' @note Updated 2019-07-24.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(indrops)
#' plotFeaturesPerCell(indrops)
NULL



#' @rdname plotFeaturesPerCell
#' @name plotFeaturesPerCell
#' @importFrom bioverbs plotFeaturesPerCell
#' @usage plotFeaturesPerCell(object, ...)
#' @export
NULL



## Updated 2019-07-24.
`plotFeaturesPerCell,SingleCellExperiment` <-  # nolint
    function(
        object,
        geom,
        interestingGroups = NULL,
        min = 0L,
        max = Inf,
        trans = "log2",
        fill,
        title = "genes per cell"
    ) {
        geom <- match.arg(geom)
        do.call(
            what = .plotQCMetric,
            args = list(
                object = object,
                metricCol = "nGene",
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
