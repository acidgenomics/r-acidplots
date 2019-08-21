#' Principal component analysis plot
#'
#' @section Principal component analysis:
#'
#' PCA (Jolliffe, et al., 2002) is a multivariate technique that allows us to
#' summarize the systematic patterns of variations in the data. PCA takes the
#' expression levels for genes and transforms it in principal component space,
#' reducing each sample into one point. Thereby, we can separate samples by
#' expression variation, and identify potential sample outliers. The PCA plot is
#' a way to look at how samples are clustering.
#'
#' @name plotPCA
#' @note Updated 2019-08-21.
#'
#' @inheritParams acidroxygen::params
#' @param ntop `integer(1)` or `Inf`.
#'   Number of most variable genes to plot.
#'   Use `Inf` to include all genes (*not recommended*).
#' @param ... Additional arguments.
#'
#' @references Jolliffe, et al., 2002.
#'
#' @seealso `DESeq2::plotPCA()`.
#'
#' We're using a modified version of the `DESeqTransform` method here.
#'
#' ```
#' methodFunction(
#'     f = "plotPCA",
#'     signature = "DESeqTransform",
#'     package = "DESeq2"
#' )
#' ```
#'
#' @return `ggplot` or `DataFrame`.
#'
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
#' plotPCA(rse, label = FALSE)
NULL



#' @rdname plotPCA
#' @name plotPCA
#' @importFrom BiocGenerics plotPCA
#' @usage plotPCA(object, ...)
#' @export
NULL



## Updated 2019-08-21.
`plotPCA,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        ntop = 500L,
        label,
        color,
        pointSize,
        title = "PCA",
        subtitle = NULL,
        return = c("ggplot", "DataFrame"),
        ...
    ) {
        ## nocov start
        call <- standardizeCall()
        ## genes
        if ("genes" %in% names(call)) {
            stop("'genes' is defunct. Use 'ntop' argument instead.")
        }
        ## samples, censorSamples
        if (any(c("samples", "censorSamples") %in% names(call))) {
            stop("Sample selection is defunct. Use bracket-based subsetting.")
        }
        ## returnData
        if ("returnData" %in% names(call)) {
            stop("'returnData' is defunct. Use 'return' argument instead.")
        }
        ## Error on unsupported arguments.
        assert(isSubset(
            x = setdiff(names(call), ""),
            y = names(formals())
        ))
        rm(call)
        ## nocov end
        validObject(object)
        assert(
            isScalar(assay),
            isInt(ntop),
            isFlag(label),
            isGGScale(color, scale = "discrete", aes = "colour", nullOK = TRUE),
            isInt(pointSize),
            isPositive(pointSize),
            isString(title, nullOK = TRUE)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        return <- match.arg(return)
        ## Warn and early return if any samples are duplicated.
        if (!hasUniqueCols(object)) {
            warning("Non-unique samples detected. Skipping plot.")
            return(invisible())
        }
        ## Handle `ntop` definition automatically.
        if (ntop > nrow(object)) {
            ntop <- nrow(object)
        }
        message(sprintf(
            "Plotting PCA using %d %s.",
            ntop,
            ngettext(
                n = ntop,
                msg1 = "feature",
                msg2 = "features"
            )
        ))
        ## Using a modified version of DESeq2 DESeqTransform method here.
        counts <- assay(object, i = assay)
        rv <- rowVars(counts)
        select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
        pca <- prcomp(t(counts[select, , drop = FALSE]))
        percentVar <- (pca[["sdev"]] ^ 2L) / (sum(pca[["sdev"]] ^ 2L))
        data <- DataFrame(
            PC1 = pca[["x"]][, 1L],
            PC2 = pca[["x"]][, 2L],
            sampleData(object)
        )
        ## Note that we're assigning the percent variation values used
        ## for the axes into the object attributes.
        attr(data, "percentVar") <- percentVar[1L:2L]
        if (return == "DataFrame") {
            return(data)
        }
        ## Plot.
        data <- as_tibble(data, rownames = NULL)
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("PC1"),
                y = !!sym("PC2"),
                color = !!sym("interestingGroups")
            )
        ) +
            geom_point(size = 4L) +
            coord_fixed() +
            labs(
                title = title,
                subtitle = subtitle,
                x = paste0(
                    "PC1: ", round(percentVar[[1L]] * 100L), "% variance"
                ),
                y = paste0(
                    "PC2: ", round(percentVar[[2L]] * 100L), "% variance"
                ),
                color = paste(interestingGroups, collapse = ":\n")
            )
        ## Color.
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
        ## Label.
        if (isTRUE(label)) {
            p <- p + acid_geom_label_repel(
                mapping = aes(label = !!sym("sampleName"))
            )
        }
        ## Return.
        p
    }

formals(`plotPCA,SummarizedExperiment`)[c("color", "label", "pointSize")] <-
    formalsList[c("color.discrete", "label", "point.size")]



#' @rdname plotPCA
#' @export
setMethod(
    f = "plotPCA",
    signature = signature("SummarizedExperiment"),
    definition = `plotPCA,SummarizedExperiment`
)
