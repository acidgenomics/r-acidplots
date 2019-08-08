#' @name plotCountsVsFeatures
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::plotCountsVsFeatures
#' @note Updated 2019-08-08.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(indrops)
#' plotCountsVsFeatures(indrops)
NULL



#' @rdname plotCountsVsFeatures
#' @name plotCountsVsFeatures
#' @importFrom bioverbs plotCountsVsFeatures
#' @usage plotCountsVsFeatures(object, ...)
#' @export
NULL



## Updated 2019-07-24.
`plotCountsVsFeatures,SingleCellExperiment` <-  # nolint
    function(
        object,
        interestingGroups = NULL,
        trendline = FALSE,
        color,
        trans = "log2",
        title = "Counts vs. features"
    ) {
        do.call(
            what = .plotQCScatterplot,
            args = list(
                object = object,
                interestingGroups = interestingGroups,
                trendline = trendline,
                ## Note that these were renamed in v0.3.19 to better match
                ## conventions used in Chromium and Seurat packages.
                xCol = "nCount",
                yCol = "nFeature",
                color = color,
                xTrans = trans,
                yTrans = trans,
                title = title
            )
        )
    }

formals(`plotCountsVsFeatures,SingleCellExperiment`)[["color"]] <-
    formalsList[["color.discrete"]]



#' @rdname plotCountsVsFeatures
#' @export
setMethod(
    f = "plotCountsVsFeatures",
    signature = signature("SingleCellExperiment"),
    definition = `plotCountsVsFeatures,SingleCellExperiment`
)
