#' @name plotNovelty
#' @author Michael Steinbaugh
#' @inherit AcidGenerics::plotNovelty
#' @note Updated 2022-01-19.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' object <- calculateMetrics(object)
#' plotNovelty(object)
NULL



## Updated 2021-09-10.
`plotNovelty,SCE` <-  # nolint
    function(
        object,
        geom,
        interestingGroups = NULL,
        min = 0L,
        trans = "identity",
        labels
    ) {
        assert(isInRightOpenRange(min, lower = 0L, upper = 1L))
        do.call(
            what = .plotQCMetric,
            args = list(
                "object" = object,
                "metricCol" = "log10FeaturesPerCount",
                "geom" = match.arg(geom),
                "interestingGroups" = interestingGroups,
                "min" = min,
                "max" = 1L,
                "trans" = trans,
                "ratio" = TRUE,
                "labels" = matchLabels(labels)
            )
        )
    }

f <- formals(`plotNovelty,SCE`)
f[["geom"]] <- .formalsList[["geom"]]
f[["labels"]] <- formals(.plotQCMetric)[["labels"]]
f[["labels"]][["title"]] <- "Novelty"
formals(`plotNovelty,SCE`) <- f



#' @rdname plotNovelty
#' @export
setMethod(
    f = "plotNovelty",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotNovelty,SCE`
)
