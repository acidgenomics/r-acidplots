#' @name plotFeaturesPerCell
#' @inherit AcidGenerics::plotFeaturesPerCell
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2022-03-07.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' plotFeaturesPerCell(object)
NULL



## Updated 2021-09-10.
`plotFeaturesPerCell,SCE` <- # nolint
    function(object,
             geom,
             interestingGroups = NULL,
             min = 0L,
             max = Inf,
             trans = "log2",
             labels) {
        do.call(
            what = .plotQCMetric,
            args = list(
                "object" = object,
                "metricCol" = "nFeature",
                "geom" = match.arg(geom),
                "interestingGroups" = interestingGroups,
                "min" = min,
                "max" = max,
                "trans" = trans,
                "labels" = matchLabels(labels)
            )
        )
    }

.f <- formals(`plotFeaturesPerCell,SCE`)
.f[["geom"]] <- .formalsList[["geom"]]
.f[["labels"]] <- formals(.plotQCMetric)[["labels"]]
.f[["labels"]][["title"]] <- "Features per cell"
.f[["labels"]][["metricAxis"]] <- "features"
formals(`plotFeaturesPerCell,SCE`) <- .f # nolint
rm(.f)



#' @rdname plotFeaturesPerCell
#' @export
setMethod(
    f = "plotFeaturesPerCell",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotFeaturesPerCell,SCE`
)
