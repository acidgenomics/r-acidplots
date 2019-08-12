#' @name plotQC
#' @inherit bioverbs::plotQC
#' @note Updated 2019-07-29.
#'
#' @inheritParams acidroxygen::params
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



## Updated 2019-08-12.
`plotQC,SummarizedExperiment` <-  # nolint
    function(object, assay = 1L) {
        validObject(object)
        assert(isScalar(assay))

        totalCounts <- plotTotalCounts(object, assay = assay)
        zerosVsDepth <- plotZerosVsDepth(object, assay = assay)

        ## Counts per row or column.
        mat <- as.matrix(assay(object, i = assay))
        rowSums <- .plotSumsECDF(mat, fun = rowSums) +
            labs(title = "Counts per row")
        colSums <- .plotSumsECDF(mat, fun = colSums) +
            labs(title = "Counts per column")

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



## Updated 2019-08-11.
`plotQC,SingleCellExperiment` <-  # nolint
    function(object) {
        ## FIXME Need to add `hasMetrics()` from goalie.
        assert(hasMetrics(object))
    }



#' @rdname plotQC
#' @export
setMethod(
    f = "plotQC",
    signature = signature("SingleCellExperiment"),
    definition = `plotQC,SingleCellExperiment`
)




## Updated 2019-08-12.
`plotQC,SingleCellExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        geom,
        legend
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isFlag(legend)
        )
        if (!hasMetrics(object, colData = c("nCount", "nFeature"))) {
            object <- calculateMetrics(object)
        }
        geom <- match.arg(geom)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)

        ## Don't show cell counts for unfiltered datasets.
        if (hasSubset(object, metadata = "filterCells")) {
            cellCounts <- plotCellCounts(object)
            zerosVsDepth <- NULL
        } else {
            cellCounts <- NULL
            zerosVsDepth <- plotZerosVsDepth(object)
        }

        countsPerCell <- plotCountsPerCell(object, geom = geom)
        featuresPerCell <- plotFeaturesPerCell(object, geom = geom)
        countsVsFeatures <- plotCountsVsFeatures(object)
        novelty <- plotNovelty(object, geom = geom)
        ## This isn't defined for all objects.
        mitoRatio <- tryCatch(
            expr = plotMitoRatio(object, geom = geom),
            error = function(e) {
                NULL
            }
        )

        plotlist <- list(
            cellCounts = cellCounts,
            countsPerCell = countsPerCell,
            featuresPerCell = featuresPerCell,
            countsVsFeatures = countsVsFeatures,
            novelty = novelty,
            mitoRatio = mitoRatio,
            zerosVsDepth = zerosVsDepth
        )

        ## Remove any `NULL` plots. This is useful for nuking the
        ## `plotReadsPerCell` return on an object that doesn't contain raw
        ## cellular barcode counts.
        plotlist <- Filter(f = Negate(is.null), x = plotlist)

        ## Hide the legends, if desired.
        if (identical(legend, FALSE)) {
            .hideLegend <- function(gg) {
                gg + theme(legend.position = "none")
            }
            plotlist <- lapply(plotlist, .hideLegend)
        }

        ## Return.
        plot_grid(plotlist = plotlist)
    }

formals(`plotQC,SingleCellExperiment`)[["geom"]] <- geom
formals(`plotQC,SingleCellExperiment`)[["legend"]] <- formalsList[["legend"]]



#' @rdname plotQC
#' @export
setMethod(
    f = "plotQC",
    signature = signature("SingleCellExperiment"),
    definition = `plotQC,SingleCellExperiment`
)
