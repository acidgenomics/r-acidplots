#' @name plotCountsVsFeatures
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotCountsVsFeatures
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
#' plotCountsVsFeatures(object)
NULL



## Updated 2021-09-10.
`plotCountsVsFeatures,SCE` <-  # nolint
    function(
        object,
        interestingGroups = NULL,
        trendline = FALSE,
        trans = "log2",
        labels = list(
            "title" = "Counts vs. features",
            "subtitle" = NULL,
            "x" = "counts",
            "y" = "features"
        )
    ) {
        do.call(
            what = .plotQCScatterplot,
            args = list(
                "object" = object,
                "interestingGroups" = interestingGroups,
                "trendline" = trendline,
                "xCol" = "nCount",
                "yCol" = "nFeature",
                "xTrans" = trans,
                "yTrans" = trans,
                "labels" = matchLabels(labels)
            )
        )
    }



#' @rdname plotCountsVsFeatures
#' @export
setMethod(
    f = "plotCountsVsFeatures",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotCountsVsFeatures,SCE`
)
