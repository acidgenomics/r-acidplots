## FIXME Move to acidplots.



#' @name plotMitoVsCoding
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::plotMitoVsCoding
#' @note Updated 2019-07-24.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(indrops)
#' plotMitoVsCoding(indrops)
NULL



#' @rdname plotMitoVsCoding
#' @name plotMitoVsCoding
#' @importFrom bioverbs plotMitoVsCoding
#' @usage plotMitoVsCoding(object, ...)
#' @export
NULL



## Updated 2019-07-24.
`plotMitoVsCoding,bcbioSingleCell` <-  # nolint
    function(
        object,
        interestingGroups = NULL,
        trendline = FALSE,
        color,
        trans = "log2",
        title = "Mito vs. coding"
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
                title = title
            )
        )
    }

formals(`plotMitoVsCoding,bcbioSingleCell`)[["color"]] <-
    formalsList[["color.discrete"]]



#' @rdname plotMitoVsCoding
#' @export
setMethod(
    f = "plotMitoVsCoding",
    signature = signature("bcbioSingleCell"),
    definition = `plotMitoVsCoding,bcbioSingleCell`
)