#' @name plotPCA
#' @inherit AcidGenerics::plotPCA
#' @note Updated 2021-02-09.
#'
#' @details
#' We're using a modified version of the `DESeqTransform` method here.
#'
#' ```r
#' methodFunction(
#'     f = "plotPCA",
#'     signature = "DESeqTransform",
#'     package = "DESeq2"
#' )
#' ```
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
#' @section `SingleCellExperiment`:
#'
#' The `SingleCellExperiment` method that visualizes dimension reduction data
#' slotted in `reducedDims()` is defined in pointillism package.
#'
#' @inheritParams AcidRoxygen::params
#' @param ntop `integer(1)` or `Inf`.
#'   Number of most variable genes to plot.
#'   Use `Inf` to include all genes (*not recommended*).
#' @param ... Additional arguments.
#'
#' @references Jolliffe, et al., 2002.
#'
#' @seealso
#' - `DESeq2::plotPCA()`.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotPCA(object)
NULL



## Updated 2021-05-17.
`plotPCA,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        ntop = 500L,
        label,
        color,
        pointSize,
        labels = list(
            title = "PCA",
            subtitle = NULL
        )
    ) {
        requireNamespaces("matrixStats")
        validObject(object)
        assert(
            isScalar(assay),
            isInt(ntop),
            isFlag(label),
            isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE),
            isInt(pointSize),
            isPositive(pointSize)
        )
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        ## Early return if any samples are duplicated.
        if (!hasUniqueCols(object)) {
            alertWarning("Non-unique samples detected. Skipping plot.")
            return(invisible(NULL))
        }
        ## Handle `ntop` definition automatically.
        if (isTRUE(ntop > nrow(object))) {
            ntop <- nrow(object)
        }
        ## Using a modified version of DESeq2 DESeqTransform method here.
        counts <- assay(object, i = assay)
        ## Make dense, if necessary, so we can calculate `rowVars`.
        counts <- as.matrix(counts)
        rv <- matrixStats::rowVars(counts)
        select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
        pca <- prcomp(t(counts[select, , drop = FALSE]))
        percentVar <- (pca[["sdev"]] ^ 2L) / (sum(pca[["sdev"]] ^ 2L))
        data <- data.frame(
            "pc1" = pca[["x"]][, 1L],
            "pc2" = pca[["x"]][, 2L],
            sampleData(object)
        )
        attr(data, "percentVar") <- percentVar[seq_len(2L)]
        ## Plot.
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("pc1"),
                y = !!sym("pc2"),
                color = str_replace_na(!!sym("interestingGroups"))
            )
        ) +
            geom_point(size = 4L) +
            coord_fixed()
        ## Labels.
        if (is.list(labels)) {
            if (is.null(labels[["subtitle"]])) {
                labels[["subtitle"]] <- paste0("n = ", ntop)
            }
            labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
            labels[["fill"]] <- labels[["color"]]
            labels[["x"]] <- paste0(
                "PC1: ", round(percentVar[[1L]] * 100L), "% variance"
            )
            labels[["y"]] <- paste0(
                "PC2: ", round(percentVar[[2L]] * 100L), "% variance"
            )
            p <- p + do.call(what = labs, args = labels)
        }
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
