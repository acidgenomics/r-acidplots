#' @name plotQC
#' @inherit bioverbs::plotQC
#' @inheritParams params
#' @examples
#' data(rse, package = "acidtest")
#' plotQC(rse)
NULL



#' @rdname plotQC
#' @name plotQC
#' @importFrom bioverbs plotQC
#' @export
NULL



# Consider exporting this as a method?
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
            x = deparse(substitute(fun)),
            y = "ECDF"
        )
}



plotQC.SummarizedExperiment <-  # nolint
    function(object, assay = 1L) {
        validObject(object)
        assert(isScalar(assay))

        # Always coerce to dense matrix.
        mat <- as.matrix(assays(object)[[assay]])

        # Total counts.
        totalCounts <- plotTotalCounts(object, assay = assay)

        # Dropout rate.
        zerosVsDepth <- plotZerosVsDepth(object, assay = assay)

        # Counts per row (gene).
        rowSums <- .plotSumsECDF(mat, fun = rowSums) +
            labs(title = "counts per row")

        # Counts per column (sample).
        colSums <- .plotSumsECDF(mat, fun = colSums) +
            labs(title = "counts per column")

        # Return paneled plot.
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
    definition = plotQC.SummarizedExperiment
)
