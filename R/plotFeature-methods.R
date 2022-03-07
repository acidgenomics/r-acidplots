#' @name plotFeature
#' @inherit AcidGenerics::plotFeature
#' @note Updated 2021-03-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param features `character`. Features to plot (e.g. gene expression, PC
#'   scores, number of genes detected).
#' @param ... Additional arguments.
#'
#' @seealso
#' - `Seurat::FeaturePlot()`.
#'
#' @return `ggplot` (1 feature) or `list` (multiple features).
#'
#' @examples
#' data(SingleCellExperiment_Seurat, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_Seurat
#' plotFeature(
#'     object = object,
#'     features = c("nCount_RNA", "nFeature_RNA", "PC_1", "PC_2"),
#'     reduction = "UMAP"
#' )
NULL



## Updated 2022-03-07.
`plotFeature,SCE` <-  # nolint
    function(
        object,
        features,
        reduction,
        color,
        pointSize,
        pointAlpha,
        pointsAsNumbers,
        label,
        labelSize,
        dark,
        legend
    ) {
        assert(
            isCharacter(features),
            isScalar(reduction),
            isGGScale(
                x = color,
                scale = "continuous",
                aes = "color",
                nullOK = TRUE
            ),
            isNumber(pointSize),
            isNumber(pointAlpha),
            isFlag(pointsAsNumbers),
            isFlag(label),
            isNumber(labelSize),
            isFlag(dark),
            isFlag(legend)
        )
        features <- camelCase(features, strict = TRUE)
        ## Dark mode setup.
        if (isTRUE(dark)) {
            fill <- "black"
        } else {
            fill <- "white"
        }
        ## Get the dimension reduction data.
        data <- .fetchReductionData(
            object = object,
            reduction = reduction
        )
        ## Label the axes.
        axes <- colnames(data)[seq_len(2L)]
        ## If the features are not defined, attempt to merge all reduced dims
        ## information before stopping.
        if (!isSubset(features, colnames(data))) {
            reductionData <- .bindReducedDims(object)
            colnames(reductionData) <-
                camelCase(colnames(reductionData), strict = TRUE)
            data <- data[, setdiff(colnames(data), colnames(reductionData))]
            data <- cbind(data, reductionData)
        }
        ## Only supporting visualization of numeric column metadata.
        supported <- bapply(X = data, FUN = is.numeric)
        supported <- names(supported)[supported]
        ## These values are required for dim reduction labeling.
        denylist <- c("centerX", "centerY", "x", "y")
        supported <- setdiff(supported, denylist)
        if (!isSubset(features, supported)) {
            setdiff <- setdiff(features, supported)
            abort(sprintf(
                fmt = paste0(
                    "%s ",
                    ngettext(
                        n = length(setdiff),
                        msg1 = "feature",
                        msg2 = "features"
                    ),
                    " not defined: %s\n",
                    "Available:\n%s"
                ),
                length(setdiff),
                toInlineString(setdiff, n = 5L),
                printString(supported)
            ))
        }
        plotlist <- lapply(
            X = features,
            FUN = function(feature) {
                p <- ggplot(
                    data = as.data.frame(data),
                    mapping = aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        color = !!sym(feature)
                    )
                )
                if (isTRUE(pointsAsNumbers)) {
                    if (pointSize < 4L) pointSize <- 4L
                    p <- p +
                        geom_text(
                            mapping = aes(
                                x = !!sym("x"),
                                y = !!sym("y"),
                                label = !!sym("ident"),
                                color = !!sym(feature)
                            ),
                            alpha = pointAlpha,
                            size = pointSize
                        )
                } else {
                    p <- p +
                        geom_point(
                            alpha = pointAlpha,
                            size = pointSize
                        )
                }
                p <- p +
                    labs(
                        color = NULL,
                        x = makeLabel(axes[[1L]]),
                        y = makeLabel(axes[[2L]]),
                        title = makeLabel(feature)
                    )
                if (isTRUE(label)) {
                    if (isTRUE(dark)) {
                        labelColor <- "white"
                    } else {
                        labelColor <- "black"
                    }
                    p <- p +
                        geom_text(
                            mapping = aes(
                                x = !!sym("centerX"),
                                y = !!sym("centerY"),
                                label = !!sym("ident")
                            ),
                            color = labelColor,
                            size = labelSize,
                            fontface = "bold"
                        )
                }
                if (isTRUE(dark)) {
                    p <- p + acid_theme_dark()
                    if (is.null(color)) {
                        color <- eval(.formalsList[["darkMarkerColors"]])
                    }
                }
                if (is(color, "ScaleContinuous")) {
                    p <- p + color
                }
                if (!isTRUE(legend)) {
                    p <- p + guides(color = "none")
                }
                p
            }
        )
        ## Return --------------------------------------------------------------
        if (isTRUE(length(features) > 1L)) {
            wrap_plots(plotlist) +
                theme(
                    plot.background = element_rect(color = NA, fill = fill)
                )
        } else {
            plotlist[[1L]]
        }
    }

args <- c(
    "dark",
    "expression",
    "label",
    "labelSize",
    "legend",
    "pointAlpha",
    "pointSize2",
    "pointsAsNumbers",
    "reduction"
)
formals(`plotFeature,SCE`)[
    c(
        "color",
        "dark",
        "expression",
        "label",
        "labelSize",
        "legend",
        "pointAlpha",
        "pointSize",
        "pointsAsNumbers",
        "reduction"
    )] <-
    .formalsList[c(
        "continuousColor",
        "dark",
        "expression",
        "label",
        "labelSize",
        "legend",
        "pointAlpha",
        "pointSize2",
        "pointsAsNumbers",
        "reduction"
    )]



#' @rdname plotFeature
#' @export
setMethod(
    f = "plotFeature",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotFeature,SCE`
)
