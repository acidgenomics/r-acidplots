#' @name plotCountsPerBroadClass
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotCountsPerBroadClass
#' @note Updated 2021-09-09.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Passthrough arguments to `plotCountsPerBiotype()`.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotCountsPerBroadClass(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotCountsPerBroadClass(object)
NULL



## Updated 2021-09-09.
`plotCountsPerBroadClass,SE` <-  # nolint
    function(
        object,
        ...,
        labels = list(
            title = "Counts per broad class biotype",
            subtitle = NULL,
            sampleAxis = NULL,
            countAxis = "counts"
        )
    ) {
        plotCountsPerBiotype(
            object = object,
            biotypeCol = "broadClass",
            labels = matchLabels(labels),
            ...
        )
    }



`plotCountsPerBroadClass,SCE` <-  # nolint
    `plotCountsPerBroadClass,SE`



#' @rdname plotCountsPerBroadClass
#' @export
setMethod(
    f = "plotCountsPerBroadClass",
    signature = signature("SingleCellExperiment"),
    definition = `plotCountsPerBroadClass,SCE`
)

#' @rdname plotCountsPerBroadClass
#' @export
setMethod(
    f = "plotCountsPerBroadClass",
    signature = signature("SummarizedExperiment"),
    definition = `plotCountsPerBroadClass,SE`
)
