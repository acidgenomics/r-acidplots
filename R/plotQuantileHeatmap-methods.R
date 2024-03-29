#' @name plotQuantileHeatmap
#' @inherit AcidGenerics::plotQuantileHeatmap
#' @note Updated 2022-03-07.
#'
#' @inheritParams plotHeatmap
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param legend `logical(1)`.
#' Show the color legend.
#'
#' @param n `integer(1)`.
#' The number of quantile breaks to create.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment_splatter,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotQuantileHeatmap(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' plotQuantileHeatmap(object)
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



## Updated 2021-02-08.
`plotQuantileHeatmap,SE` <- # nolint
    function(object,
             assay = 1L,
             interestingGroups = NULL,
             n = 10L,
             clusterRows = TRUE,
             clusterCols = TRUE,
             showRownames = isTRUE(nrow(object) <= 30L),
             showColnames = TRUE,
             treeheightRow = 50L,
             treeheightCol = 50L,
             color,
             legendColor,
             legend = FALSE,
             borderColor = NULL,
             title = NULL,
             ## Attept to map genes to symbols automatically only when shown.
             convertGenesToSymbols = showRownames,
             ...) {
        requireNamespaces("pheatmap")
        validObject(object)
        assert(
            nrow(object) > 1L,
            ncol(object) > 1L,
            isScalar(assay),
            isInt(n),
            isFlag(clusterCols),
            isFlag(clusterRows),
            isFlag(legend),
            isString(borderColor, nullOk = TRUE),
            isString(title, nullOk = TRUE),
            isFlag(convertGenesToSymbols)
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
            alertWarning("Non-unique samples detected. Skipping plot.")
            return(invisible(NULL))
        }
        ## Modify the object to use gene symbols in the row names automatically,
        ## if possible. We're using `tryCatch()` call here to return the object
        ## unmodified if gene symbols aren't defined.
        if (isTRUE(convertGenesToSymbols)) {
            object <- tryCatch(
                expr = suppressMessages({
                    convertGenesToSymbols(object)
                }),
                error = function(e) {
                    object
                }
            )
        }
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
            error = function(e) {
                NULL
            }
        )
        if (hasLength(sampleNames)) {
            colnames(mat) <- sampleNames
            if (hasLength(annotationCol) && !anyNA(annotationCol)) {
                rownames(annotationCol) <- sampleNames
            }
        }
        ## Return pretty heatmap with modified defaults.
        args <- list(
            "mat" = mat,
            "annotationCol" = annotationCol,
            "annotationColors" = annotationColors,
            "borderColor" = borderColor,
            "breaks" = breaks,
            "clusterCols" = clusterCols,
            "clusterRows" = clusterRows,
            "color" = color,
            "legend" = legend,
            "legendBreaks" = breaks,
            "legendLabels" = round(breaks, digits = 2L),
            "main" = title,
            "scale" = "none",
            "showColnames" = showColnames,
            "showRownames" = showRownames,
            "treeheightCol" = treeheightCol,
            "treeheightRow" = treeheightRow,
            ...
        )
        args <- .pheatmapArgs(args)
        ## Ignore "partial match of 'just' to 'justification'" warning.
        withCallingHandlers(
            expr = do.call(what = pheatmap::pheatmap, args = args),
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
        )
    }

formals(`plotQuantileHeatmap,SE`)[c("color", "legendColor")] <- # nolint
    .formalsList[c("heatmapQuantileColor", "heatmapLegendColor")]



## Updated 2020-02-19.
`plotQuantileHeatmap,SCE` <- # nolint
    function(object, ...) {
        plotQuantileHeatmap(
            object = aggregateCellsToSamples(object),
            ...
        )
    }



#' @rdname plotQuantileHeatmap
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotQuantileHeatmap,SCE`
)

#' @rdname plotQuantileHeatmap
#' @export
setMethod(
    f = "plotQuantileHeatmap",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotQuantileHeatmap,SE`
)
