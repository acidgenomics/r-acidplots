#' @name plotQC
#' @inherit bioverbs::plotQC
#' @note Updated 2019-07-29.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' plotQC(rse)
NULL



#' @rdname plotQC
#' @name plotQC
#' @importFrom bioverbs plotQC
#' @usage plotQC(object, ...)
#' @export
NULL



## Consider exporting this as a method?
## Updated 2019-07-23.
.plotSumsECDF <- function(object, fun) {
    assert(is.function(fun))
    data <- tibble(x = fun(object))
    ggplot(
        data = data,
        mapping = aes(x = !!sym("x"))
    ) +
        stat_ecdf(size = 1L) +
        scale_x_continuous(trans = "sqrt") +
        labs(
            x = makeLabel(deparse(substitute(fun))),
            y = "ECDF"
        )
}



## Updated 2019-07-23.
`plotQC,SummarizedExperiment` <-  # nolint
    function(object, assay = 1L) {
        validObject(object)
        assert(isScalar(assay))

        ## Always coerce to dense matrix.
        mat <- as.matrix(assays(object)[[assay]])

        ## Total counts.
        totalCounts <- plotTotalCounts(object, assay = assay)

        ## Dropout rate.
        zerosVsDepth <- plotZerosVsDepth(object, assay = assay)

        ## Counts per row (gene).
        rowSums <- .plotSumsECDF(mat, fun = rowSums) +
            labs(title = "counts per row")

        ## Counts per column (sample).
        colSums <- .plotSumsECDF(mat, fun = colSums) +
            labs(title = "counts per column")

        ## Return paneled plot.
        plot_grid(
            plotlist = list(
                totalCounts = totalCounts,
                zerosVsDepth = zerosVsDepth,
                rowSums = rowSums,
                colSums = colSums
            )
        )
    }



#' @rdname plotQC
#' @export
setMethod(
    f = "plotQC",
    signature = signature("SummarizedExperiment"),
    definition = `plotQC,SummarizedExperiment`
)
