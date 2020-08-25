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
#' [`hclust()`][stats::hclust], and defaults to the Ward method.
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
#' recommended as an alternate approach to be used with
#' [`pheatmap()`][pheatmap::pheatmap], which is called internally by our
#' plotting code. Here's how this can be accomplished:
#'
#' ```
#' mat <- assay(mat)
#' dist <- dist(mat)
#' hclust <- hclust(dist, method = "ward.D2")
#' ```
#'
#' @name plotHeatmap
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2020-08-05.
#'
#' @inheritParams acidroxygen::params
#' @param scale `character(1)`.
#'   Whether the values should be centered and scaled in either the row or
#'   column direction, or remain unscaled.
#' @param breaks `numeric` or `NULL`.
#'   A sequence of numbers that covers the range of values in the matrix. Must
#'   be 1 element longer than the color vector, which is handled internally
#'   automatically, differing from the behavior in pheatmap.
#' @param clusteringMethod `character(1)`.
#'   Clustering method. Accepts the same values as [`hclust()`][stats::hclust].
#' @param clusterRows,clusterCols `logical(1)`.
#'   Arrange with hierarchical clustering.
#' @param color `function`, `character`, or `NULL`.
#'   Hexadecimal color function or values to use for plot.
#'
#'   We generally recommend these hexadecimal functions from the viridis
#'   package, in addition to our [synesthesia()] palette:
#'
#'   - `viridis::viridis()`.
#'   - `viridis::inferno()`.
#'   - `viridis::magma()`.
#'   - `viridis::plasma()`.
#'
#'   Alternatively, colors can be defined manually using hexadecimal values
#'   (e.g. `c("#FF0000", "#0000FF")`), but this is not generally recommended.
#'   Refer to the RColorBrewer package for hexadecimal color palettes that may
#'   be suitable. If set `NULL`, will use the default pheatmap colors.
#' @param legendBreaks `numeric` or `NULL`.
#'   Numeric vector of breakpoints for the color legend.
#' @param legendColor `function` or `NULL`.
#'   Hexadecimal color function to use for legend labels. Note that hexadecimal
#'   values are not supported. If set `NULL`, will use the default pheatmap
#'   colors.
#' @param showRownames,showColnames `logical(1)`.
#'   Show row or column names.
#' @param treeheightRow,treeheightCol `integer(1)`.
#'   Size of the row and column dendrograms. Use `0` to disable.
#' @param ... Passthrough arguments to [`pheatmap()`][pheatmap::pheatmap].
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
#' data(
#'     RangedSummarizedExperiment,
#'     package = "acidtest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' ## Row scaling requires non-zero rows.
#' object <- nonzeroRowsAndCols(object)
#'
#' ## Symmetric row-scaled breaks (recommended).
#' plotHeatmap(
#'     object,
#'     scale = "row",
#'     color = acidplots::blueYellow,
#'     breaks = seq(from = -2L, to = 2L, by = 0.25),
#'     legendBreaks = seq(from = -2L, to = 2L, by = 1L)
#' )
#'
#' ## Using custom hexadecimal color input.
#' color <- rev(RColorBrewer::brewer.pal(n = 11L, name = "PuOr"))
#' color <- grDevices::colorRampPalette(color)
#' plotHeatmap(
#'     object,
#'     scale = "row",
#'     color = color,
#'     breaks = seq(from = -2L, to = 2L, by = 0.25),
#'     legendBreaks = seq(from = -2L, to = 2L, by = 1L)
#' )
NULL



#' @rdname plotHeatmap
#' @name plotHeatmap
#' @importFrom acidgenerics plotHeatmap
#' @usage plotHeatmap(object, ...)
#' @export
NULL



## Updated 2020-08-05.
`plotHeatmap,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        scale = c("row", "column", "none"),
        clusteringMethod = "ward.D2",
        clusterRows = TRUE,
        clusterCols = TRUE,
        showRownames = isTRUE(nrow(object) <= 30L),
        showColnames = TRUE,
        ## Set to `0L` to disable.
        treeheightRow = 50L,
        ## Set to `0L` to disable.
        treeheightCol = 50L,
        color,
        legendColor,
        breaks = seq(from = -3L, to = 3L, by = 0.25),
        legendBreaks = seq(from = -3L, to = 3L, by = 1L),
        borderColor = NULL,
        title = NULL,
        ## Attept to map genes to symbols automatically only when shown.
        convertGenesToSymbols = showRownames,
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
            isString(title, nullOK = TRUE),
            isFlag(convertGenesToSymbols)
        )
        scale <- match.arg(scale)
        if (!isString(borderColor)) borderColor <- NA
        if (!isString(title)) title <- NA
        if (!is.numeric(breaks)) breaks <- NA
        if (!is.numeric(legendBreaks)) legendBreaks <- NA
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        ## Warn and early return if any samples are duplicated.
        ## We've included this step here to work with the minimal bcbio RNA-seq
        ## test data set, which contains duplicate samples.
        if (!hasUniqueCols(object)) {
            cli_alert_warning("Non-unique samples detected. Skipping plot.")
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
                error = function(e) object
            )
        }
        ## Ensure we're always using a dense matrix.
        mat <- as.matrix(assay(object, i = assay))
        ## Ensure the user isn't passing in a matrix with any rows or columns
        ## containing all zeros when we're attempting to z-scale.
        if (!identical(scale, "none")) {
            assert(hasNonzeroRowsAndCols(mat))
        }
        ## Pre-process the matrix by applying row/column scaling, if desired.
        ## Run this step before hierarchical clustering (i.e. calculating the
        ## distance matrix).
        mat <- .scaleMatrix(mat, scale = scale)
        ## Now we're ready to perform hierarchical clustering. Generate `hclust`
        ## objects for rows and columns that we'll pass to pheatmap. Note that
        ## pheatmap supports `clusterRows = TRUE` and `clusterCols = TRUE`, but
        ## these have been found to error for some datasets. Therefore, we're
        ## performing hclust calculations on own here.
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
        args <- list(color = color)
        if (is.numeric(breaks)) {
            args[["n"]] <- length(breaks) - 1L
        }
        color <- do.call(what = .pheatmapColorPalette, args = args)
        ## Substitute human-friendly sample names, if defined.
        sampleNames <- tryCatch(
            expr = sampleNames(object),
            error = function(e) NULL
        )
        if (hasLength(sampleNames)) {
            colnames(mat) <- sampleNames
            if (hasLength(annotationCol) && !any(is.na(annotationCol))) {
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
            clusterCols = hc[["cols"]],
            clusterRows = hc[["rows"]],
            color = color,
            legendBreaks = legendBreaks,
            main = title,
            ## We're already applied scaling manually (see above).
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

formals(`plotHeatmap,SummarizedExperiment`)[["color"]] <-
    formalsList[["heatmap.color"]]
formals(`plotHeatmap,SummarizedExperiment`)[["legendColor"]] <-
    formalsList[["heatmap.legend.color"]]



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("SummarizedExperiment"),
    definition = `plotHeatmap,SummarizedExperiment`
)



## Updated 2020-02-19.
`plotHeatmap,SingleCellExperiment` <-  # nolint
    function(object, ...) {
        plotHeatmap(
            object = aggregateCellsToSamples(object),
            ...
        )
    }



#' @rdname plotHeatmap
#' @export
setMethod(
    f = "plotHeatmap",
    signature = signature("SingleCellExperiment"),
    definition = `plotHeatmap,SingleCellExperiment`
)
