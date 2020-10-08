#' @name plotQC
#' @inherit acidgenerics::plotQC
#' @note Updated 2019-08-12.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
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
#' plotQC(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- calculateMetrics(object)
#' plotQC(object, legend = FALSE)
NULL



#' @rdname plotQC
#' @name plotQC
#' @importFrom acidgenerics plotQC
#' @usage plotQC(object, ...)
#' @export
NULL



## Updated 2019-08-12.
`plotQC,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        legend
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isFlag(legend)
        )
        totalCounts <- plotTotalCounts(object, assay = assay)
        zerosVsDepth <- plotZerosVsDepth(object, assay = assay)
        rowSums <- plotSums(object, assay = assay, MARGIN = 1L)
        colSums <- plotSums(object, assay = assay, MARGIN = 2L)
        plotlist <- list(
            totalCounts = totalCounts,
            zerosVsDepth = zerosVsDepth,
            rowSums = rowSums,
            colSums = colSums
        )
        plotlist <- Filter(f = Negate(is.null), x = plotlist)
        ## Hide the legends, if desired.
        if (identical(legend, FALSE)) {
            plotlist <- .hideLegendsInPlotlist(plotlist)

        }
        ## Return as grid.
        plot_grid(plotlist = plotlist)
    }

formals(`plotQC,SummarizedExperiment`)[["legend"]] <- formalsList[["legend"]]



#' @rdname plotQC
#' @export
setMethod(
    f = "plotQC",
    signature = signature("SummarizedExperiment"),
    definition = `plotQC,SummarizedExperiment`
)



## Updated 2019-08-12.
`plotQC,SingleCellExperiment` <-  # nolint
    function(
        object,
        geom,
        legend
    ) {
        validObject(object)
        assert(
            hasMetrics(object),
            identical(assayNames(object)[[1L]], "counts"),
            isFlag(legend)
        )
        geom <- match.arg(geom)
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
        mitoRatio <-
            tryCatch(
                expr = plotMitoRatio(object, geom = geom),
                error = function(e) NULL
            )
        rowSums <- plotSums(object, MARGIN = 1L)
        colSums <- plotSums(object, MARGIN = 2L)
        plotlist <- list(
            cellCounts = cellCounts,
            countsPerCell = countsPerCell,
            featuresPerCell = featuresPerCell,
            countsVsFeatures = countsVsFeatures,
            novelty = novelty,
            mitoRatio = mitoRatio,
            zerosVsDepth = zerosVsDepth,
            rowSums = rowSums,
            colSums = colSums
        )
        plotlist <- Filter(f = Negate(is.null), x = plotlist)
        ## Hide the legends, if desired.
        if (identical(legend, FALSE)) {
            plotlist <- .hideLegendsInPlotlist(plotlist)
        }
        ## Return as grid.
        plot_grid(plotlist = plotlist)
    }

formals(`plotQC,SingleCellExperiment`)[["geom"]] <- .geom
formals(`plotQC,SingleCellExperiment`)[["legend"]] <- formalsList[["legend"]]



#' @rdname plotQC
#' @export
setMethod(
    f = "plotQC",
    signature = signature("SingleCellExperiment"),
    definition = `plotQC,SingleCellExperiment`
)
