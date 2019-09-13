#' @name plotNovelty
#' @author Michael Steinbaugh
#' @inherit bioverbs::plotNovelty
#' @note Updated 2019-07-27.
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
#' plotNovelty(object)
NULL



#' @rdname plotNovelty
#' @name plotNovelty
#' @importFrom bioverbs plotNovelty
#' @usage plotNovelty(object, ...)
#' @export
NULL



## Updated 2019-07-27.
`plotNovelty,SingleCellExperiment` <-  # nolint
    function(
        object,
        geom,
        interestingGroups = NULL,
        min = 0L,
        fill,
        trans = "identity",
        title = "Novelty"
    ) {
        assert(isInRightOpenRange(min, lower = 0L, upper = 1L))
        geom <- match.arg(geom)
        do.call(
            what = .plotQCMetric,
            args = list(
                object = object,
                metricCol = "log10FeaturesPerCount",
                geom = geom,
                interestingGroups = interestingGroups,
                min = min,
                max = 1L,
                trans = trans,
                ratio = TRUE,
                fill = fill,
                title = title
            )
        )
    }

formals(`plotNovelty,SingleCellExperiment`)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(`plotNovelty,SingleCellExperiment`)[["geom"]] <- .geom



#' @rdname plotNovelty
#' @export
setMethod(
    f = "plotNovelty",
    signature = signature("SingleCellExperiment"),
    definition = `plotNovelty,SingleCellExperiment`
)
