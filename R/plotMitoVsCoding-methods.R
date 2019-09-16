#' @name plotMitoVsCoding
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::plotMitoVsCoding
#' @note Updated 2019-07-24.
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
#' if (!anyNA(object$nMito)) {
#'     plotMitoVsCoding(object)
#' }
NULL



#' @rdname plotMitoVsCoding
#' @name plotMitoVsCoding
#' @importFrom bioverbs plotMitoVsCoding
#' @usage plotMitoVsCoding(object, ...)
#' @export
NULL



## Updated 2019-07-24.
`plotMitoVsCoding,SingleCellExperiment` <-  # nolint
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
                labels = labels
            )
        )
    }

f <- formals(`plotMitoVsCoding,SingleCellExperiment`)
f[["color"]] <- formalsList[["color.discrete"]]
formals(`plotMitoVsCoding,SingleCellExperiment`) <- f



#' @rdname plotMitoVsCoding
#' @export
setMethod(
    f = "plotMitoVsCoding",
    signature = signature("SingleCellExperiment"),
    definition = `plotMitoVsCoding,SingleCellExperiment`
)
