#' @name plotMitoVsCoding
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotMitoVsCoding
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
#' if (!anyNA(object$nMito)) {
#'     plotMitoVsCoding(object)
#' }
NULL



#' @rdname plotMitoVsCoding
#' @name plotMitoVsCoding
#' @importFrom AcidGenerics plotMitoVsCoding
#' @usage plotMitoVsCoding(object, ...)
#' @export
NULL



## Updated 2019-09-16.
`plotMitoVsCoding,SCE` <-  # nolint
    function(
        object,
        interestingGroups = NULL,
        trendline = FALSE,
        color,
        trans = "log2",
        labels = list(
            title = "Mito vs. coding",
            subtitle = NULL,
            x = "coding",
            y = "mito"
        )
    ) {
        do.call(
            what = .plotQCScatterplot,
            args = list(
                object = object,
                interestingGroups = interestingGroups,
                trendline = trendline,
                xCol = "nCoding",
                yCol = "nMito",
                color = color,
                xTrans = trans,
                yTrans = trans,
                labels = matchLabels(labels)
            )
        )
    }

f <- formals(`plotMitoVsCoding,SCE`)
f[["color"]] <- formalsList[["color.discrete"]]
formals(`plotMitoVsCoding,SCE`) <- f



#' @rdname plotMitoVsCoding
#' @export
setMethod(
    f = "plotMitoVsCoding",
    signature = signature("SingleCellExperiment"),
    definition = `plotMitoVsCoding,SCE`
)
