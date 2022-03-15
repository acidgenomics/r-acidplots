#' @name plotMitoVsCoding
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotMitoVsCoding
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
#' try({
#'     plotMitoVsCoding(object)
#' })
NULL



#' @rdname plotMitoVsCoding
#' @name plotMitoVsCoding
#' @importFrom AcidGenerics plotMitoVsCoding
#' @usage plotMitoVsCoding(object, ...)
#' @export
NULL



## Updated 2021-09-10.
`plotMitoVsCoding,SCE` <- # nolint
    function(object,
             interestingGroups = NULL,
             trendline = FALSE,
             trans = "log2",
             labels = list(
                 "title" = "Mito vs. coding",
                 "subtitle" = NULL,
                 "x" = "coding",
                 "y" = "mito"
             )) {
        do.call(
            what = .plotQCScatterplot,
            args = list(
                "object" = object,
                "interestingGroups" = interestingGroups,
                "trendline" = trendline,
                "xCol" = "nCoding",
                "yCol" = "nMito",
                "xTrans" = trans,
                "yTrans" = trans,
                "labels" = matchLabels(labels)
            )
        )
    }



#' @rdname plotMitoVsCoding
#' @export
setMethod(
    f = "plotMitoVsCoding",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotMitoVsCoding,SCE`
)
