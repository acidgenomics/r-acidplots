#' @name plotCountsCorrelationHeatmap
#' @inherit AcidGenerics::plotCountsCorrelationHeatmap
#' @note Updated 2019-07-29.
#'
#' @inheritParams base::Extract
#' @inheritParams stats::cor
#' @inheritParams AcidRoxygen::params
#' @param ... Passthrough arguments to
#'   [`plotHeatmap()`][AcidPlots::plotHeatmap].
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## matrix ====
#' x <- SummarizedExperiment::assay(RangedSummarizedExperiment)
#' y <- x + 1L
#' plotCountsCorrelationHeatmap(x, y)
NULL



#' @rdname plotCountsCorrelationHeatmap
#' @name plotCountsCorrelationHeatmap
#' @importFrom AcidGenerics plotCountsCorrelationHeatmap
#' @usage plotCountsCorrelationHeatmap(x, y, ...)
#' @export
NULL



## Updated 2019-07-23.
`plotCountsCorrelationHeatmap,matrix` <-  # nolint
    function(
        x,
        y,
        i = NULL,
        j = NULL,
        method = "pearson",
        ...
    ) {
        validObject(x)
        validObject(y)
        assert(
            identical(dimnames(x), dimnames(y)),
            !anyNA(x), !anyNA(y),
            isString(method)
        )
        if (!is.null(i)) {
            x <- x[i, , drop = FALSE]
            y <- y[i, , drop = FALSE]
        }
        if (!is.null(j)) {
            x <- x[, j, drop = FALSE]
            y <- y[, j, drop = FALSE]
        }
        cor <- cor(x = x, y = y, method = method)
        se <- SummarizedExperiment(assays = list(cor = cor))
        plotHeatmap(
            object = se,
            scale = "none",
            clusterRows = FALSE,
            clusterCols = FALSE,
            showRownames = TRUE,
            showColnames = TRUE,
            ...
        )
    }



#' @rdname plotCountsCorrelationHeatmap
#' @export
setMethod(
    f = "plotCountsCorrelationHeatmap",
    signature = signature(
        x = "matrix",
        y = "matrix"
    ),
    definition = `plotCountsCorrelationHeatmap,matrix`
)
