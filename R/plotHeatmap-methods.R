#' Heatmap
#'
#' Construct a simple heatmap.
#'
#' @section Scaling:
#'
#' Here we're scaling simply by calculating the standard score (z-score).
#'
#' - mu: mean.
#' - sigma: standard deviation.
#' - x: raw score (e.g. count matrix).
#' - z: standard score (z-score).
#'
#' ```
#' z = (x - mu) / sigma
#' ```
#'
#' See also:
#'
#' - `pheatmap:::scale_rows()`.
#' - `scale()` for additional scaling approaches.
#'
#' @section Hierarchical clustering:
#'
#' Row- and column-wise hierarchical clustering is performed when `clusterRows`
#' and/or `clusterCols` are set to `TRUE`. Internally, this calls
#' [stats::hclust()], and defaults to the Ward method.
#'
#' Automatic hierarchical clustering of rows and/or columns can error for some
#' datasets. When this occurs, you'll likely see this error:
#'
#' ```
#' Error in hclust(d, method = method) :
#' NA/NaN/Inf in foreign function call
#' ```
#'
#' In this case, either set `clusterRows` and/or `clusterCols` to `FALSE`, or
#' you can attempt to pass an `hclust` object to these arguments. This is
#' recommended as an alternate approach to be used with [pheatmap::pheatmap()],
#' which is called internally by our plotting code. Here's how this can be
#' accomplished:
#'
#' ```
#' mat <- assay(mat)
#' dist <- dist(mat)
#' hclust <- hclust(dist, method = "ward.D2")
#' ```
#'
#' @name plotHeatmap
#' @author Michael Steinbaugh, Rory Kirchner
#' @inheritParams params
#'
#' @param ... Passthrough arguments to [pheatmap::pheatmap()].
#'   The argument names must be formatted in camel case, not snake case.
#'
#' @seealso
#' - `pheatmap::pheatmap()`.
#' - `RColorBrewer::brewer.pal()`.
#' - `stats::cor()`.
#' - `stats::hclust()`.
#'
#' @return `pheatmap`.
#'
#' @examples
#' data(rse, sce, package = "acidtest")
#'
#' ## SummarizedExperiment ====
#' plotHeatmap(rse)
#'
#' ## Disable column clustering.
#' plotHeatmap(rse, clusterCols = FALSE)
#'
#' ## Using pheatmap default colors.
#' plotHeatmap(rse, color = NULL, legendColor = NULL)
#'
#' ## Using hexadecimal color input.
#' color <- RColorBrewer::brewer.pal(n = 11L, name = "PuOr")
#' color <- grDevices::colorRampPalette(color)(256L)
#' plotHeatmap(rse, color = color)
#'
#' ## SingleCellExperiment ====
#' plotHeatmap(sce)
NULL



#' @rdname plotHeatmap
#' @name plotHeatmap
#' @importFrom bioverbs plotHeatmap
#' @export
NULL



plotHeatmap.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        scale = c("none", "row", "column"),
        clusteringMethod = "ward.D2",
        clusterRows = FALSE,
        clusterCols = FALSE,
        showRownames = FALSE,
        showColnames = TRUE,
        # Set to `0L` to disable.
        treeheightRow = 50L,
        # Set to `0L` to disable.
        treeheightCol = 50L,
        color,
        legendColor,
        borderColor = NULL,
        title = NULL,
        ...
    ) {
        validObject(object)
        assert(
            nrow(object) > 1L,
            ncol(object) > 1L,
            isScalar(assay),
            isFlag(clusterRows),
            isFlag(clusterCols),
            isFlag(showRownames),
            isFlag(showColnames),
            isInt(treeheightRow),
            isInt(treeheightCol),
            isString(borderColor, nullOK = TRUE),
            isString(title, nullOK = TRUE)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        scale <- match.arg(scale)
        if (!isString(borderColor)) {
            borderColor <- NA
        }
        if (!isString(title)) {
            title <- NA
        }

        # Warn and early return if any samples are duplicated.
        # We've included this step here to work with the minimal bcbio RNA-seq
        # test data set, which contains duplicate samples.
        if (!hasUniqueCols(object)) {
            warning("Non-unique samples detected. Skipping plot.")
            return(invisible())
        }

        # Modify the object to use gene symbols in the row names automatically,
        # if possible. We're using `tryCatch()` call here to return the object
        # unmodified if gene symbols aren't defined.
        object <- tryCatch(
            expr = suppressMessages(
                convertGenesToSymbols(object)
            ),
            error = function(e) object
        )

        # Ensure we're always using a dense matrix.
        mat <- as.matrix(assays(object)[[assay]])

        # Ensure the user isn't passing in a matrix with any rows or columns
        # containing all zeros when we're attempting to z-scale.
        if (scale != "none") {
            assert(hasNonZeroRowsAndCols(mat))
        }

        # Pre-process the matrix by applying row/column scaling, if desired.
        # Run this step before hierarchical clustering (i.e. calculating the
        # distance matrix).
        mat <- .scaleMatrix(mat, scale = scale)

        # Now we're ready to perform hierarchical clustering. Generate `hclust`
        # objects for rows and columns that we'll pass to pheatmap. Note that
        # pheatmap supports `clusterRows = TRUE` and `clusterCols = TRUE`, but
        # these have been found to error for some datasets. Therefore, we're
        # performing hclust calculations on own here.
        hc <- .hclust(
            object = mat,
            method = clusteringMethod,
            rows = clusterRows,
            cols = clusterCols
        )
        assert(
            is.list(hc),
            identical(names(hc), c("rows", "cols"))
        )

        # Get annotation columns and colors automatically.
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
        color <- .pheatmapColorPalette(color)

        # Substitute human-friendly sample names, if defined.
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

        # Return pretty heatmap with modified defaults.
        args <- list(
            mat = mat,
            annotationCol = annotationCol,
            annotationColors = annotationColors,
            borderColor = borderColor,
            clusterCols = hc[["cols"]],
            clusterRows = hc[["rows"]],
            color = color,
            main = title,
            # We're already applied scaling manually (see above).
            scale = "none",
            showColnames = showColnames,
            showRownames = showRownames,
            treeheightCol = treeheightCol,
            treeheightRow = treeheightRow,
            ...
        )
        args <- .pheatmapArgs(args)
        do.call(what = pheatmap, args = args)
    }

formals(plotHeatmap.SummarizedExperiment)[["color"]] <-
    formalsList[["acid.heatmap.color"]]
formals(plotHeatmap.SummarizedExperiment)[["legendColor"]] <-
    formalsList[["acid.heatmap.color"]]



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = plotHeatmap.SummarizedExperiment
)



plotHeatmap.SingleCellExperiment <-  # nolint
    function(object) {
        agg <- aggregateCellsToSamples(object, fun = "mean")
        do.call(
            what = plotHeatmap,
            args = matchArgsToDoCall(
                args = list(object = agg)
            )
        )
    }

formals(plotHeatmap.SingleCellExperiment) <-
    formals(plotHeatmap.SummarizedExperiment)



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = plotHeatmap.SingleCellExperiment
)