#' @name plotCorrelationHeatmap
#' @inherit AcidGenerics::plotCorrelationHeatmap
#' @note Updated 2021-02-08.
#'
#' @inheritParams plotHeatmap
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param method `character(1)`.
#' Correlation coefficient (or covariance) method to be computed.
#' Defaults to pearson, but spearman or kendall can also be used.
#' Refer to `cor()` documentation for details.
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
#' plotCorrelationHeatmap(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' plotCorrelationHeatmap(object)
NULL



## Updated 2022-03-07.
`plotCorrelationHeatmap,SE` <- # nolint
    function(object,
             assay = 1L,
             interestingGroups = NULL,
             method,
             clusteringMethod = "ward.D2",
             showRownames = TRUE,
             showColnames = TRUE,
             treeheightRow = 0L,
             treeheightCol = 50L,
             color,
             legendColor,
             borderColor = NULL,
             title = TRUE,
             ...) {
        assert(
            requireNamespaces("pheatmap"),
            validObject(object),
            isScalar(assay),
            nrow(object) > 1L,
            ncol(object) > 1L,
            isString(clusteringMethod),
            isFlag(showRownames),
            isFlag(showColnames),
            isInt(treeheightRow),
            isInt(treeheightCol),
            isString(borderColor, nullOK = TRUE)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        method <- match.arg(method)
        if (!isString(borderColor)) {
            borderColor <- NA
        }
        if (isTRUE(title)) {
            title <- paste(method, "correlation")
        } else if (!isString(title)) {
            title <- NA
        }
        ## Early return if any samples are duplicated.
        if (!hasUniqueCols(object)) {
            alertWarning("Non-unique samples detected. Skipping plot.")
            return(invisible(NULL))
        }
        ## Correlation matrix.
        mat <- as.matrix(assay(object, i = assay))
        ## Inform the user if NA values are present, and replace with zeros.
        if (anyNA(mat)) {
            alertWarning(sprintf(
                "%d NA detected in matrix. Replacing with zeros.",
                sum(is.na(mat))
            ))
            mat[is.na(mat)] <- 0L
        }
        alert(sprintf(
            "Calculating correlation matrix using {.var %s} method.",
            method
        ))
        cor <- cor(x = mat, y = NULL, method = method)
        ## Check for NA values in correlation matrix and error, if necessary.
        assert(
            !anyNA(cor),
            msg = "NA values detected in correlation matrix."
        )
        ## Get annotation columns and colors automatically.
        x <- .pheatmapAnnotations(object = object, legendColor = legendColor)
        assert(
            is.list(x),
            identical(
                x = names(x),
                y = c("annotationCol", "annotationColors")
            )
        )
        annotationCol <- x[["annotationCol"]]
        annotationColors <- x[["annotationColors"]]
        color <- .pheatmapColorPalette(color = color)
        ## Substitute human-friendly sample names, if defined.
        sampleNames <- tryCatch(
            expr = sampleNames(object),
            error = function(e) {
                NULL
            }
        )
        if (hasLength(sampleNames)) {
            rownames(cor) <- sampleNames
            colnames(cor) <- sampleNames
            if (hasLength(annotationCol) && !anyNA(annotationCol)) {
                rownames(annotationCol) <- sampleNames
            }
        }
        ## Return pretty heatmap with modified defaults.
        args <- list(
            "mat" = cor,
            "annotationCol" = annotationCol,
            "annotationColors" = annotationColors,
            "borderColor" = borderColor,
            "breaks" = NULL,
            "clusteringMethod" = clusteringMethod,
            "clusteringDistanceCols" = "correlation",
            "clusteringDistanceRows" = "correlation",
            "color" = color,
            "legendBreaks" = NULL,
            "main" = title,
            "scale" = "none",
            "showColnames" = showColnames,
            "showRownames" = showRownames,
            "treeheightCol" = treeheightCol,
            "treeheightRow" = treeheightRow,
            ...
        )
        args <- .pheatmapArgs(args)
        do.call(what = pheatmap::pheatmap, args = args)
    }

formals(`plotCorrelationHeatmap,SE`)[c( # nolint
    "color",
    "legendColor",
    "method"
)] <- list(
    .formalsList[["heatmapCorrelationColor"]],
    .formalsList[["heatmapLegendColor"]],
    formals(stats::cor)[["method"]]
)



## Updated 2020-02-19.
`plotCorrelationHeatmap,SCE` <- # nolint
    function(object, ...) {
        plotCorrelationHeatmap(
            object = aggregateCellsToSamples(object),
            ...
        )
    }



#' @rdname plotCorrelationHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotCorrelationHeatmap,SCE`
)

#' @rdname plotCorrelationHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotCorrelationHeatmap,SE`
)
