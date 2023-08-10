#' @name plotReducedDim
#' @aliases plotPCA plotTSNE plotUMAP
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotReducedDim
#' @note Updated 2022-03-07.
#'
#' @details
#' For `SingleCellExperiment`, colors using `ident` column defined in
#' `colData()` by default.
#'
#' @section Reduction types:
#'
#' - PCA: **P**rincipal **C**omponent **A**nalysis.
#' - t-SNE: **t**-distributed **S**tochastic **N**eighbor **E**mbedding.
#' - UMAP: **U**niform **M**anifold **A**pproximation and **P**rojection.
#'
#' @section Principal component analysis (`plotPCA`):
#'
#' PCA (Jolliffe, et al., 2002) is a multivariate technique that allows us to
#' summarize the systematic patterns of variations in the data. PCA takes the
#' expression levels for genes and transforms it in principal component space,
#' reducing each sample into one point. Thereby, we can separate samples by
#' expression variation, and identify potential sample outliers. The PCA plot is
#' a way to look at how samples are clustering.
#'
#' We're using a modified version of the `DESeqTransform` method here.
#'
#' @section UMAP calculation:
#'
#' [UMAP][] calculation in R requires the [Python][] module `umap-learn`.
#' The UMAP module can be loaded in R using [reticulate][].
#'
#' [Python]: https://www.python.org
#' [UMAP]: https://github.com/lmcinnes/umap
#' [reticulate]: https://rstudio.github.io/reticulate/
#'
#' @inheritParams AcidRoxygen::params
#' @param ntop `integer(1)` or `Inf`.
#' Number of most variable genes to plot.
#' Use `Inf` to include all genes (*not recommended*).
#' @param ... Additional arguments.
#'
#' @references Jolliffe, et al., 2002.
#'
#' @seealso
#' - `DESeq2::plotPCA()`.
#' - `Seurat::DimPlot()`.
#' - `monocle3::plot_cells()`.
#'
#' @return `ggplot`.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment_Seurat,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotPCA(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_Seurat
#' plotReducedDim(object, reduction = "UMAP")
NULL



## Updated 2021-09-10.
`plotPCA,SE` <- # nolint
    function(object,
             assay = 1L,
             interestingGroups = NULL,
             ntop = 500L,
             label,
             pointSize,
             labels = list(
                 "title" = "PCA",
                 "subtitle" = NULL
             )) {
        requireNamespaces("matrixStats")
        validObject(object)
        assert(
            isScalar(assay),
            isInt(ntop),
            isFlag(label),
            isInt(pointSize),
            isPositive(pointSize)
        )
        labels <- matchLabels(labels)
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
        percentVar <- (pca[["sdev"]]^2L) / (sum(pca[["sdev"]]^2L))
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
                x = .data[["pc1"]],
                y = .data[["pc2"]],
                color = str_replace_na(.data[["interestingGroups"]])
            )
        ) +
            geom_point(size = 4L) +
            coord_fixed()
        ## Labels.
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
        ## Color palette.
        p <- p + autoDiscreteColorScale()
        ## Label.
        if (isTRUE(label)) {
            p <- p + acid_geom_label_repel(
                mapping = aes(label = .data[["sampleName"]])
            )
        }
        ## Return.
        p
    }

formals(`plotPCA,SE`)[c( # nolint
    "label",
    "pointSize"
)] <- .formalsList[c(
    "label",
    "pointSize"
)]



## Updated 2022-03-07.
`plotReducedDim,SCE` <- # nolint
    function(object,
             reduction,
             dims,
             interestingGroups = NULL,
             color,
             pointSize,
             pointAlpha,
             pointsAsNumbers,
             label,
             labelSize,
             dark,
             legend,
             labels = list(
                 "title" = NULL,
                 "subtitle" = NULL
             )) {
        validObject(object)
        assert(
            hasClusters(object),
            ## Allow pass in of positional scalar, for looping.
            isScalar(reduction),
            hasLength(dims, n = 2L),
            all(isIntegerish(dims)),
            isCharacter(interestingGroups, nullOK = TRUE),
            isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE),
            isNumber(pointSize),
            isNumber(pointAlpha),
            isFlag(pointsAsNumbers),
            isFlag(label),
            isNumber(labelSize),
            isFlag(dark),
            isFlag(legend)
        )
        labels <- matchLabels(labels)
        dl(c(
            "reduction" = reduction,
            "dims" = deparse(dims)
        ))
        ## Note that we're not slotting interesting groups back into object
        ## here because we're allowing visualization of cluster identity, which
        ## isn't sample level.
        if (is.null(interestingGroups)) {
            interestingGroups <- "ident"
        }
        data <- .fetchReductionData(
            object = object,
            reduction = reduction,
            dims = dims
        )
        assert(
            is(data, "DFrame"),
            isSubset(
                x = c("x", "y", "centerX", "centerY", "interestingGroups"),
                y = colnames(data)
            )
        )
        ## Check if interesting groups input is supported.
        supported <- bapply(data, is.factor)
        supported <- names(supported)[supported]
        denylist <- c("interestingGroups", "origIdent", "sampleId")
        supported <- setdiff(supported, denylist)
        if (!isSubset(interestingGroups, supported)) {
            setdiff <- setdiff(interestingGroups, supported)
            abort(sprintf(
                fmt = paste0(
                    "%s ",
                    ngettext(
                        n = length(setdiff),
                        msg1 = "interesting group",
                        msg2 = "interesting groups"
                    ),
                    " not defined: %s\n",
                    "Available:\n%s"
                ),
                length(setdiff),
                toInlineString(setdiff, n = 5L),
                printString(supported)
            ))
        }
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = interestingGroups
        )
        ## Turn off labeling if there's only 1 cluster.
        if (hasLength(levels(data[["ident"]]), n = 1L)) {
            label <- FALSE
        }
        ## Set the x- and y-axis labels (e.g. t_SNE1, t_SNE2). We're setting
        ## this up internally as the first two columns in the data frame.
        axes <- colnames(data)[seq_len(2L)]
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[["x"]],
                y = .data[["y"]],
                color = .data[["interestingGroups"]]
            )
        )
        ## Points as numbers.
        if (isTRUE(pointsAsNumbers)) {
            ## Increase the size, if necessary.
            if (pointSize < 4L) {
                pointSize <- 4L
            }
            p <- p +
                geom_text(
                    mapping = aes(
                        x = .data[["x"]],
                        y = .data[["y"]],
                        label = .data[["ident"]],
                        color = .data[["interestingGroups"]]
                    ),
                    alpha = pointAlpha,
                    size = pointSize,
                    show.legend = legend
                )
        } else {
            p <- p +
                geom_point(
                    alpha = pointAlpha,
                    size = pointSize,
                    show.legend = legend
                )
        }
        ## Label.
        if (isTRUE(label)) {
            if (isTRUE(dark)) {
                labelColor <- "white"
            } else {
                labelColor <- "black"
            }
            p <- p +
                geom_text(
                    mapping = aes(
                        x = .data[["centerX"]],
                        y = .data[["centerY"]],
                        label = .data[["ident"]]
                    ),
                    color = labelColor,
                    size = labelSize,
                    fontface = "bold"
                )
        }
        ## Dark mode.
        if (isTRUE(dark)) {
            p <- p + acid_theme_dark()
        }
        ## Color.
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
        ## Improve the axis breaks.
        p <- p +
            scale_x_continuous(breaks = pretty_breaks(n = 4L)) +
            scale_y_continuous(breaks = pretty_breaks(n = 4L))
        ## Labels.
        if (is.list(labels)) {
            labels[["x"]] <- makeLabel(axes[[1L]])
            labels[["y"]] <- makeLabel(axes[[2L]])
            labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
            labels[["fill"]] <- labels[["color"]]
            p <- p + do.call(what = labs, args = labels)
        }
        p
    }

formals(`plotReducedDim,SCE`)[c( # nolint
    "color",
    "dark",
    "dims",
    "label",
    "labelSize",
    "legend",
    "pointAlpha",
    "pointSize",
    "pointsAsNumbers",
    "reduction"
)] <- .formalsList[c(
    "discreteColor",
    "dark",
    "dims",
    "label",
    "labelSize",
    "legend",
    "pointAlpha",
    "pointSize2",
    "pointsAsNumbers",
    "reduction"
)]



## Updated 2020-02-21.
`plotPCA,SCE` <- # nolint
    function(object, ...) {
        plotReducedDim(object = object, reduction = "PCA", ...)
    }

## Updated 2020-02-21.
`plotTSNE,SCE` <- # nolint
    function(object, ...) {
        plotReducedDim(object = object, reduction = "TSNE", ...)
    }

## Updated 2020-02-21.
`plotUMAP,SCE` <- # nolint
    function(object, ...) {
        plotReducedDim(object = object, reduction = "UMAP", ...)
    }



#' @rdname plotReducedDim
#' @export
setMethod(
    f = "plotReducedDim",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotReducedDim,SCE`
)

#' @rdname plotReducedDim
#' @export
setMethod(
    f = "plotPCA",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotPCA,SCE`
)

#' @rdname plotReducedDim
#' @export
setMethod(
    f = "plotPCA",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotPCA,SE`
)

#' @rdname plotReducedDim
#' @export
setMethod(
    f = "plotTSNE",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotTSNE,SCE`
)

#' @rdname plotReducedDim
#' @export
setMethod(
    f = "plotUMAP",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotUMAP,SCE`
)
