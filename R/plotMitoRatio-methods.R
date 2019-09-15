#' @name plotMitoRatio
#' @author Michael Steinbaugh, Rory Kirchner
#' @include plotQC-internal.R
#' @inherit bioverbs::plotMitoRatio
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
#' if (!anyNA(object$mitoRatio)) {
#'     plotMitoRatio(object)
#' }
NULL



#' @rdname plotMitoRatio
#' @name plotMitoRatio
#' @importFrom bioverbs plotMitoRatio
#' @usage plotMitoRatio(object, ...)
#' @export
NULL



## Updated 2019-09-15.
`plotMitoRatio,SingleCellExperiment` <-  # nolint
    function(
        object,
        geom,
        interestingGroups = NULL,
        max = 1L,
        fill,
        trans = "sqrt",
        labels
    ) {
        assert(isInLeftOpenRange(max, lower = 0L, upper = 1L))
        geom <- match.arg(geom)
        do.call(
            what = .plotQCMetric,
            args = list(
                object = object,
                metricCol = "mitoRatio",
                geom = geom,
                interestingGroups = interestingGroups,
                max = max,
                trans = trans,
                ratio = TRUE,
                fill = fill,
                labels = labels
            )
        )
    }

f <- formals(`plotMitoRatio,SingleCellExperiment`)
f[["fill"]] <- formalsList[["fill.discrete"]]
f[["geom"]] <- .geom
f[["labels"]] <- formals(.plotQCMetric)[["labels"]]
f[["labels"]][["title"]] <- "Mito ratio"
formals(`plotMitoRatio,SingleCellExperiment`) <- f



#' @rdname plotMitoRatio
#' @export
setMethod(
    f = "plotMitoRatio",
    signature = signature("SingleCellExperiment"),
    definition = `plotMitoRatio,SingleCellExperiment`
)
