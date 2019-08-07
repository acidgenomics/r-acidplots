#' @name plotCorrelationHeatmap
#' @inherit bioverbs::plotCorrelationHeatmap
#' @note Updated 2019-07-29.
#'
#' @inheritParams plotHeatmap
#' @inheritParams acidroxygen::params
#' @param method `character(1)`.
#'   Correlation coefficient (or covariance) method to be computed. Defaults to
#'   "`pearson`" but "`spearman`" or "`kendall`" can also be used. Refer to the
#'   [stats::cor()] documentation for details.
#' @param ... Additional arguments.
#'
#' @return `pheatmap`.
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#' rse <- RangedSummarizedExperiment
#' sce <- SingleCellExperiment
#'
#' ## SummarizedExperiment ====
#' plotCorrelationHeatmap(rse)
#'
#' ## SingleCellExperiment ====
#' plotCorrelationHeatmap(sce)
NULL



#' @rdname plotCorrelationHeatmap
#' @name plotCorrelationHeatmap
#' @importFrom bioverbs plotCorrelationHeatmap
#' @usage plotCorrelationHeatmap(object, ...)
#' @export
NULL



## Updated 2019-07-23.
`plotCorrelationHeatmap,SummarizedExperiment` <-  # nolint
    function(
        object,
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
        ...
    ) {
        validObject(object)
        assert(
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
        ## Warn and early return if any samples are duplicated.
        if (!hasUniqueCols(object)) {
            warning("Non-unique samples detected. Skipping plot.")
            return(invisible())
        }

        ## Correlation matrix.
        mat <- as.matrix(assays(object)[[assay]])

        ## Inform the user if NA values are present, and replace with zeros.
        if (any(is.na(mat))) {
            message(paste(
                sum(is.na(mat)),
                "NA values detected in matrix.",
                "Replacing with zeros."
            ))
            mat[is.na(mat)] <- 0L
        }

        message(paste(
            "Calculating correlation matrix using", method, "method."
        ))
        cor <- cor(x = mat, y = NULL, method = method)

        ## Check for NA values in correlation matrix and error, if necessary.
        if (any(is.na(cor))) {
            stop("NA values detected in correlation matrix.")
        }

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
            error = function(e) NULL
        )
        if (length(sampleNames) > 0L) {
            rownames(cor) <- sampleNames
            colnames(cor) <- sampleNames
            if (
                length(annotationCol) > 0L &&
                !any(is.na(annotationCol))
            ) {
                rownames(annotationCol) <- sampleNames
            }
        }

        ## Return pretty heatmap with modified defaults.
        args <- list(
            mat = cor,
            annotationCol = annotationCol,
            annotationColors = annotationColors,
            borderColor = borderColor,
            clusteringMethod = clusteringMethod,
            clusteringDistanceCols = "correlation",
            clusteringDistanceRows = "correlation",
            color = color,
            main = title,
            showColnames = showColnames,
            showRownames = showRownames,
            treeheightCol = treeheightCol,
            treeheightRow = treeheightRow,
            ...
        )
        args <- .pheatmapArgs(args)
        assert(areDisjointSets(names(args), "scale"))
        do.call(what = pheatmap, args = args)
    }

formals(`plotCorrelationHeatmap,SummarizedExperiment`)[["method"]] <-
    formals(stats::cor)[["method"]]
formals(`plotCorrelationHeatmap,SummarizedExperiment`)[["color"]] <-
    formalsList[["heatmap.color"]]
formals(`plotCorrelationHeatmap,SummarizedExperiment`)[["legendColor"]] <-
    formalsList[["heatmap.color"]]



#' @rdname plotCorrelationHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = `plotCorrelationHeatmap,SummarizedExperiment`
)



## Updated 2019-07-23.
`plotCorrelationHeatmap,SingleCellExperiment` <-  # nolint
    function(object) {
        agg <- aggregateCellsToSamples(object, fun = "mean")
        do.call(
            what = plotCorrelationHeatmap,
            args = matchArgsToDoCall(
                args = list(object = agg)
            )
        )
    }

formals(`plotCorrelationHeatmap,SingleCellExperiment`) <-
    formals(`plotCorrelationHeatmap,SummarizedExperiment`)



#' @rdname plotCorrelationHeatmap
#' @export
setMethod(
    f = "plotCorrelationHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = `plotCorrelationHeatmap,SingleCellExperiment`
)
