## FIXME Ensure we set the breaks, and color here by default.



#' @name plotQuantileHeatmap
#' @inherit bioverbs::plotQuantileHeatmap
#' @note Updated 2019-08-27.
#'
#' @inheritParams plotHeatmap
#' @inheritParams acidroxygen::params
#' @param legend `logical(1)`.
#'   Show the color legend.
#' @param n `integer(1)`.
#'   The number of quantile breaks to create.
#' @param ... Additional arguments.
#'
#' @return `pheatmap`.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotQuantileHeatmap(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotQuantileHeatmap(object)
NULL



#' @rdname plotQuantileHeatmap
#' @name plotQuantileHeatmap
#' @importFrom bioverbs plotQuantileHeatmap
#' @usage plotQuantileHeatmap(object, ...)
#' @export
NULL



## Updated 2019-07-23.
.quantileBreaks <- function(object, n = 10L) {
    assert(
        is.matrix(object),
        isInt(n),
        isPositive(n)
    )
    breaks <- quantile(object, probs = seq(0L, 1L, length.out = n))
    breaks[!duplicated(breaks)]
}



## Updated 2019-07-23.
`plotQuantileHeatmap,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        n = 10L,
        clusterRows = TRUE,
        clusterCols = TRUE,
        showRownames = FALSE,
        showColnames = TRUE,
        treeheightRow = 0L,
        treeheightCol = 50L,
        color,
        legendColor,
        legend = FALSE,
        borderColor = NULL,
        title = NULL,
        ...
    ) {
        validObject(object)
        assert(
            nrow(object) > 1L,
            ncol(object) > 1L,
            isScalar(assay),
            isInt(n),
            isFlag(clusterCols),
            isFlag(clusterRows),
            isFlag(legend),
            isString(borderColor, nullOK = TRUE),
            isString(title, nullOK = TRUE)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        n <- as.integer(n)
        if (!isString(borderColor)) {
            borderColor <- NA
        }
        if (!isString(title)) {
            title <- NA
        }
        ## Warn and early return if any samples are duplicated.
        if (!hasUniqueCols(object)) {
            warning("Non-unique samples detected. Skipping plot.")
            return(invisible())
        }
        ## Modify the object to use gene symbols in the row names automatically,
        ## if possible. We're using `tryCatch()` call here to return the object
        ## unmodified if gene symbols aren't defined.
        object <- tryCatch(
            expr = suppressMessages(
                convertGenesToSymbols(object)
            ),
            error = function(e) object
        )
        ## Ensure we're using a dense matrix.
        mat <- as.matrix(assay(object, i = assay))
        ## Calculate the quantile breaks.
        breaks <- .quantileBreaks(mat, n = n)
        ## Get annotation columns and colors automatically.
        x <- .pheatmapAnnotations(object = object, legendColor = legendColor)
        assert(
            is.list(x),
            identical(names(x), c("annotationCol", "annotationColors"))
        )
        annotationCol <- x[["annotationCol"]]
        annotationColors <- x[["annotationColors"]]
        ## Note the number of breaks here.
        color <- .pheatmapColorPalette(color = color, n = length(breaks) - 1L)
        ## Substitute human-friendly sample names, if defined.
        sampleNames <- tryCatch(
            expr = sampleNames(object),
            error = function(e) NULL
        )
        if (length(sampleNames) > 0L) {
            colnames(mat) <- sampleNames
            if (
                length(annotationCol) > 0L &&
                !any(is.na(annotationCol))
            ) {
                rownames(annotationCol) <- sampleNames
            }
        }
        ## Return pretty heatmap with modified defaults.
        args <- list(
            mat = mat,
            annotationCol = annotationCol,
            annotationColors = annotationColors,
            borderColor = borderColor,
            breaks = breaks,
            clusterCols = clusterCols,
            clusterRows = clusterRows,
            color = color,
            legend = legend,
            legendBreaks = breaks,
            legendLabels = round(breaks, digits = 2L),
            main = title,
            scale = "none",
            showColnames = showColnames,
            showRownames = showRownames,
            treeheightCol = treeheightCol,
            treeheightRow = treeheightRow,
            ...
        )
        args <- .pheatmapArgs(args)
        ## Ignore "partial match of 'just' to 'justification'" warning.
        withCallingHandlers(
            expr = do.call(what = pheatmap, args = args),
            ## nocov start
            warning = function(w) {
                if (isTRUE(grepl(
                    pattern = "partial match",
                    x = as.character(w)
                ))) {
                    invokeRestart("muffleWarning")
                } else {
                    w
                }
            }
            ## nocov end
        )
    }

formals(`plotQuantileHeatmap,SummarizedExperiment`)[["color"]] <-
    formalsList[["heatmap.color"]]
formals(`plotQuantileHeatmap,SummarizedExperiment`)[["legendColor"]] <-
    formalsList[["heatmap.color"]]



#' @rdname plotQuantileHeatmap
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = `plotQuantileHeatmap,SummarizedExperiment`
)



## Updated 2019-08-21.
`plotQuantileHeatmap,SingleCellExperiment` <-  # nolint
    function(object, ...) {
        plotQuantileHeatmap(
            object = pseudobulk(object),
            ...
        )
    }



#' @describeIn plotQuantileHeatmap Applies [pseudobulk()] calculation to average
#'   gene expression at sample level prior to plotting.\cr
#'   Passes `...` to `SummarizedExperiment` method.
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = `plotQuantileHeatmap,SingleCellExperiment`
)
