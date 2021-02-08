#' @name plotCountsVsFeatures
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotCountsVsFeatures
#' @note Updated 2019-09-16
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
#' plotCountsVsFeatures(object)
NULL



## Updated 2019-09-16.
`plotCountsVsFeatures,SingleCellExperiment` <-  # nolint
    function(
        object,
        interestingGroups = NULL,
        trendline = FALSE,
        color,
        trans = "log2",
        labels = list(
            title = "Counts vs. features",
            subtitle = NULL,
            x = "counts",
            y = "features"
        )
    ) {
        do.call(
            what = .plotQCScatterplot,
            args = list(
                object = object,
                interestingGroups = interestingGroups,
                trendline = trendline,
                xCol = "nCount",
                yCol = "nFeature",
                color = color,
                xTrans = trans,
                yTrans = trans,
                labels = matchLabels(
                    labels = labels,
                    choices = eval(formals()[["labels"]])
                )
            )
        )
    }

f <- formals(`plotCountsVsFeatures,SingleCellExperiment`)
f[["color"]] <- formalsList[["color.discrete"]]
formals(`plotCountsVsFeatures,SingleCellExperiment`) <- f



#' @rdname plotCountsVsFeatures
#' @export
setMethod(
    f = "plotCountsVsFeatures",
    signature = signature("SingleCellExperiment"),
    definition = `plotCountsVsFeatures,SingleCellExperiment`
)
