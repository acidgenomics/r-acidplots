#' @name plotMitoRatio
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotMitoRatio
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
#' try({
#'     plotMitoRatio(object)
#' })
NULL



## Updated 2021-09-10.
`plotMitoRatio,SCE` <- # nolint
    function(object,
             geom,
             interestingGroups = NULL,
             max = 1L,
             trans = "sqrt",
             labels) {
        assert(isInLeftOpenRange(max, lower = 0L, upper = 1L))
        do.call(
            what = .plotQcMetric,
            args = list(
                "object" = object,
                "metricCol" = "mitoRatio",
                "geom" = match.arg(geom),
                "interestingGroups" = interestingGroups,
                "max" = max,
                "trans" = trans,
                "ratio" = TRUE,
                "labels" = matchLabels(labels)
            )
        )
    }

.f <- formals(`plotMitoRatio,SCE`)
.f[["geom"]] <- .formalsList[["geom"]]
.f[["labels"]] <- formals(.plotQcMetric)[["labels"]]
.f[["labels"]][["title"]] <- "Mito ratio"
formals(`plotMitoRatio,SCE`) <- .f # nolint
rm(.f)



#' @rdname plotMitoRatio
#' @export
setMethod(
    f = "plotMitoRatio",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotMitoRatio,SCE`
)
