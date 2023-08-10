#' @name plotMarker
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotMarker
#' @note Updated 2023-08-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' requireNamespace("Matrix", quietly = TRUE)
#' requireNamespace("SingleCellExperiment", quietly = TRUE)
#' data(SingleCellExperiment_Seurat, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_Seurat
#' counts <- SingleCellExperiment::counts(object)
#' sums <- sort(Matrix::rowSums(counts), decreasing = TRUE)
#' genes <- names(head(sums, n = 4L))
#' plotMarker(
#'     object = object,
#'     genes = genes,
#'     reduction = "UMAP"
#' )
NULL



## Updated 2023-08-10.
`plotMarker,SCE` <- # nolint
    function(object,
             genes,
             reduction,
             expression,
             color,
             pointSize,
             pointAlpha,
             pointsAsNumbers,
             label,
             labelSize,
             dark,
             legend,
             labels = list(
                 "title" = "auto",
                 "subtitle" = NULL
             )) {
        assert(
            isCharacter(genes),
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
        labels <- matchLabels(labels)
        geneNames <- mapGenesToSymbols(object, genes)
        expression <- match.arg(expression)
        ## Fetch reduced dimension data.
        assay <- "logcounts"
        data <- .fetchReductionExpressionData(
            object = object,
            genes = genes,
            reduction = reduction,
            assay = assay
        )
        ## Get the axis labels.
        axes <- colnames(data)[seq_len(2L)]
        assert(allAreMatchingRegex(x = axes, pattern = "\\d+$"))
        requiredCols <- c(
            axes,
            "centerX",
            "centerY",
            "ident",
            "mean",
            "sum",
            "x",
            "y"
        )
        assert(isSubset(requiredCols, colnames(data)))
        ## Plot.
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data$x,
                y = .data$y,
                color = !!sym(expression)
            )
        )
        ## Customize legend.
        if (isTRUE(legend)) {
            if (isString(genes)) {
                guideTitle <- assay
            } else {
                guideTitle <- sprintf("%s\n(%s)", assay, expression)
            }
            p <- p + guides(color = guide_colorbar(title = guideTitle))
        } else {
            p <- p + guides(color = "none")
        }
        ## Points as numbers.
        if (isTRUE(pointsAsNumbers)) {
            if (pointSize < 4L) {
                pointSize <- 4L
            }
            p <- p +
                geom_text(
                    mapping = aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        label = !!sym("ident"),
                        color = !!sym(expression)
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
        ## Label clusters.
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
        ## Dark mode.
        if (isTRUE(dark)) {
            p <- p + acid_theme_dark()
            if (is.null(color)) {
                color <- eval(.formalsList[["darkMarkerColors"]])
            }
        }
        ## Color.
        if (is(color, "ScaleContinuous")) {
            p <- p + color
        }
        ## Labels.
        if (is.list(labels)) {
            ## Title (and subtitle).
            if (identical(labels[["title"]], "auto")) {
                labels[["title"]] <- toString(geneNames, width = 50L)
            }
            labels[["x"]] <- axes[[1L]]
            labels[["y"]] <- axes[[2L]]
            p <- p + do.call(what = labs, args = labels)
        }
        ## Return.
        p
    }

formals(`plotMarker,SCE`)[c( # nolint
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
)] <- .formalsList[c(
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



#' @rdname plotMarker
#' @export
setMethod(
    f = "plotMarker",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotMarker,SCE`
)
