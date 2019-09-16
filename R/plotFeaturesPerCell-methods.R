#' @name plotFeaturesPerCell
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::plotFeaturesPerCell
#' @note Updated 2019-09-15.
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



## Updated 2019-09-15.
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
                labels = labels
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
