#' @name plotNovelty
#' @author Michael Steinbaugh
#' @inherit AcidGenerics::plotNovelty
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
#' plotNovelty(object)
NULL



## Updated 2019-09-16.
`plotNovelty,SingleCellExperiment` <-  # nolint
    function(
        object,
        geom,
        interestingGroups = NULL,
        min = 0L,
        fill,
        trans = "identity",
        labels
    ) {
        assert(isInRightOpenRange(min, lower = 0L, upper = 1L))
        do.call(
            what = .plotQCMetric,
            args = list(
                object = object,
                metricCol = "log10FeaturesPerCount",
                geom = match.arg(geom),
                interestingGroups = interestingGroups,
                min = min,
                max = 1L,
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

f <- formals(`plotNovelty,SingleCellExperiment`)
f[["fill"]] <- formalsList[["fill.discrete"]]
f[["geom"]] <- .geom
f[["labels"]] <- formals(.plotQCMetric)[["labels"]]
f[["labels"]][["title"]] <- "Novelty"
formals(`plotNovelty,SingleCellExperiment`) <- f



#' @rdname plotNovelty
#' @export
setMethod(
    f = "plotNovelty",
    signature = signature("SingleCellExperiment"),
    definition = `plotNovelty,SingleCellExperiment`
)
