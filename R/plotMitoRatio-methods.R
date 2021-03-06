#' @name plotMitoRatio
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotMitoRatio
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
#' if (!anyNA(object$mitoRatio)) {
#'     plotMitoRatio(object)
#' }
NULL



## Updated 2019-09-16.
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
        do.call(
            what = .plotQCMetric,
            args = list(
                object = object,
                metricCol = "mitoRatio",
                geom = match.arg(geom),
                interestingGroups = interestingGroups,
                max = max,
                trans = trans,
                ratio = TRUE,
                fill = fill,
                labels = matchLabels(
                    labels = labels,
                    choices = eval(formals()[["labels"]])
                )
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
