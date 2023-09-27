#' @name plotCounts
#' @inherit AcidGenerics::plotCounts
#' @note Updated 2022-05-06.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param genes `character` or `missing`. Gene identifiers. The function will
#' automatically match identifiers corresponding to the rownames of the
#' object, or gene symbols defined in the object.
#'
#' @param line `character(1)`.
#' Include average (median, mean, or geometric mean) line for each interesting
#' group. Disabled by default and if samples are colored by sample name.
#'
#' @param style `character(1)`.
#' Plot style.
#'
#' @param sort `logical(1)`.
#' Sort the genes alphabetically.
#' This setting applies to the gene symbols rather than the gene identifiers
#' when `convertGenesToSymbols` is `TRUE`.
#'
#' @param colMin `numeric(1)`.
#' Minimum scaled average expression threshold. Everything smaller will be
#' set to this.
#'
#' @param colMax `numeric(1)`.
#' Maximum scaled average expression threshold. Everything larger will be set
#' to this.
#'
#' @param dotMin `numeric(1)`.
#' The fraction of cells at which to draw the smallest dot. All cell groups
#' with less than this expressing the given gene will have no dot drawn.
#'
#' @param dotScale `numeric(1)`.
#' Scale the size of the points, similar to `cex`.
#'
#' @param geom `character(1)`.
#' Plot type. Uses [`match.arg()`][base::match.arg] to pick the type.
#' Currently supports `"dots"` and `"violin"`.
#'
#' @param scale `character(1)`.
#' If "area" (default), all violins have the same area (before trimming the
#' tails). If "count", areas are scaled proportionally to the number of
#' observartions. If "width", all violins have the same maximum width.
#' See `ggplot2::geom_violin` for details.
#'
#' @return
#' - `style = "facet"`: `ggplot` grouped by `sampleName`, with
#' `ggplot2::facet_wrap()` applied to panel the samples.
#' - `style = "wide"`: `ggplot` in wide format, with genes on the x-axis.
#'
#' @seealso
#' - `Seurat::DotPlot()`.
#' - `Seurat::VlnPlot()`.
#' - `Seurat::RidgePlot()`.
#' - `monocle3::plot_genes_violin()`.
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
#' rownames <- head(rownames(object))
#' print(rownames)
#' g2s <- AcidGenomes::GeneToSymbol(object)
#' geneIds <- head(g2s[[1L]])
#' print(geneIds)
#' geneNames <- head(g2s[[2L]])
#' print(geneNames)
#'
#' ## Rownames, gene IDs, and gene names (symbols) are supported.
#' plotCounts(object, genes = geneIds, style = "facet")
#' plotCounts(object, genes = geneNames, style = "wide")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_Seurat
#'
#' ## Plotting with either gene IDs or gene names (symbols) works.
#' genes <- head(rownames(object), n = 4L)
#' print(genes)
#'
#' ## Per sample mode enabled.
#' plotCounts(object, genes = genes, perSample = TRUE)
#'
#' ## Per sample mode disabled.
#' plotCounts(object, genes = genes, perSample = FALSE)
NULL



#' Improved gene point geom
#'
#' @note Updated 2019-07-23.
#' @noRd
.genePoint <-
    function(size = 3L,
             alpha = 1L,
             ...) {
        geom_point(
            ...,
            size = size,
            alpha = alpha,
            position = position_jitterdodge(dodge.width = 0.9)
        )
    }



#' Min max
#'
#' @note Updated 2019-08-03.
#' @seealso `Seurat:::MinMax`.
#' @noRd
.minMax <- function(x, min, max) {
    x[x > max] <- max
    x[x < min] <- min
    x
}



#' Percent above
#'
#' @note Updated 2019-07-31.
#' @seealso `Seurat:::PercentAbove()`.
#' @noRd
.percentAbove <- function(x, threshold) {
    length(x[x > threshold]) / length(x)
}



#' Facet wrap the counts plot
#'
#' @note Updated 2022-05-06.
#' @noRd
.plotCountsFacet <- function(data) {
    ggplot(
        data = as.data.frame(data),
        mapping = aes(
            x = .data[["interestingGroups"]],
            y = .data[["value"]],
            color = .data[["interestingGroups"]],
            fill = .data[["interestingGroups"]]
        )
    ) +
        facet_wrap(facets = vars(.data[["rowname"]]), scales = "free_y")
}



#' Display the counts plot in wide format
#'
#' @note Updated 2022-05-06.
#' @noRd
.plotCountsWide <- function(data) {
    ggplot(
        data = as.data.frame(data),
        mapping = aes(
            x = .data[["rowname"]],
            y = .data[["value"]],
            color = .data[["interestingGroups"]],
            fill = .data[["interestingGroups"]]
        )
    )
}



## nolint start
#' Plot counts
#'
#' @note Updated 2022-05-06.
#' @noRd
#'
#' @details
#' Coercing to `SummarizedExperiment` internally for fast subsetting.
#'
#' @seealso
#' Useful posts regarding error bars:
#' - https://stackoverflow.com/a/32091916/3911732
#' - http://environmentalcomputing.net/plotting-with-ggplot-bar-plots-with-error-bars/
## nolint end
`plotCounts,SE` <- # nolint
    function(object,
             genes,
             assay = 1L,
             interestingGroups = NULL,
             convertGenesToSymbols = TRUE,
             geom = c("point", "violin", "boxplot", "bar"),
             trans = c("identity", "log2", "log10"),
             line = c("none", "median", "mean", "geometricMean"),
             legend,
             style = c("facet", "wide"),
             sort = FALSE,
             labels = list(
                 "title" = NULL,
                 "subtitle" = NULL,
                 "sampleAxis" = NULL,
                 "countAxis" = "counts"
             )) {
        validObject(object)
        if (missing(genes)) {
            genes <- rownames(object)
        }
        assert(
            isCharacter(genes),
            all(isInClosedRange(length(genes), lower = 1L, upper = 20L)),
            isScalar(assay),
            isFlag(convertGenesToSymbols),
            isFlag(legend),
            isFlag(sort)
        )
        geom <- match.arg(geom)
        trans <- match.arg(trans)
        line <- match.arg(line)
        style <- match.arg(style)
        if (identical(geom, "bar")) {
            assert(identical(style, "facet"))
        }
        labels <- matchLabels(labels)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        object <- as.SummarizedExperiment(object)
        ## This supports objects that don't contain gene-to-symbol mappings.
        genes <- mapGenesToRownames(object, genes = genes, strict = FALSE)
        assay <- assay(object, i = assay)
        assays(object) <- SimpleList(assay = assay)
        object <- object[genes, , drop = FALSE]
        if (isTRUE(convertGenesToSymbols)) {
            object <- tryCatch(
                expr = {
                    suppressMessages({
                        object <- convertGenesToSymbols(object)
                    })
                },
                error = function(e) {
                    object
                }
            )
        }
        data <- melt(
            object = object,
            min = -Inf,
            minMethod = "absolute",
            trans = trans
        )
        data <- decode(data)
        assert(isSubset("rowname", colnames(data)))
        if (isTRUE(sort)) {
            data[["rowname"]] <- as.character(data[["rowname"]])
        } else {
            assert(is.factor(data[["rowname"]]))
        }
        p <- do.call(
            what = switch(
                EXPR = style,
                "facet" = .plotCountsFacet,
                "wide" = .plotCountsWide
            ),
            args = list("data" = data)
        )
        p <- switch(
            EXPR = geom,
            "point" = p + .genePoint(show.legend = legend),
            "violin" = p + geom_violin(color = NA),
            "boxplot" = p + geom_boxplot(color = "black"),
            "bar" = p +
                stat_summary(
                    fun = mean,
                    geom = "bar",
                    color = NA
                ) +
                stat_summary(
                    fun.min = function(x) mean(x) - sem(x),
                    fun.max = function(x) mean(x) + sem(x),
                    fun = mean,
                    geom = "errorbar",
                    color = "black",
                    width = 0.15
                )
        )
        if (
            !identical(line, "none") &&
                !identical(interestingGroups, "sampleName")
        ) {
            alertInfo(sprintf("Line denotes {.fun %s}.", line))
            lineFun <- get(x = line, inherits = TRUE)
            assert(is.function(lineFun))
            p <- p + stat_summary(
                fun = lineFun,
                fun.min = lineFun,
                fun.max = lineFun,
                geom = "crossbar",
                show.legend = FALSE,
                width = 0.5
            )
        }
        ## Color palette.
        p <- p + acid_scale_color_discrete()
        p <- p + acid_scale_fill_discrete()
        ## Labels.
        if (!identical(trans, "identity")) {
            labels[["countAxis"]] <- paste(trans, labels[["countAxis"]])
        }
        labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
        labels[["fill"]] <- labels[["color"]]
        names(labels)[names(labels) == "sampleAxis"] <- "x"
        names(labels)[names(labels) == "countAxis"] <- "y"
        p <- p + do.call(what = labs, args = labels)
        ## Return.
        p
    }

formals(`plotCounts,SE`)[["legend"]] <- # nolint
    .formalsList[["legend"]]



## Updated 2022-03-07.
`plotCounts,SCE` <- # nolint
    function(object,
             genes,
             assay = c("logcounts", "normcounts"),
             geom = c("violin", "dots"),
             perSample = TRUE,
             legend,
             title = NULL) {
        validObject(object)
        assay <- match.arg(assay)
        geom <- match.arg(geom)
        args <- as.list(sys.call(which = -1L))[-1L]
        args[["geom"]] <- NULL
        switch(
            EXPR = geom,
            "dots" = {
                assert(identical(assay, "logcounts"))
                args[["assay"]] <- NULL
                what <- plotDots
            },
            "violin" = {
                what <- plotViolin
            }
        )
        do.call(what = what, args = args)
    }

formals(`plotCounts,SCE`)[["legend"]] <- # nolint
    .formalsList[["legend"]]



## Updated 2022-03-21.
`plotDots,SCE` <- # nolint
    function(object,
             genes,
             perSample = TRUE,
             colMin = -2.5,
             colMax = 2.5,
             dotMin = 0L,
             dotScale = 6L,
             color,
             legend,
             title = NULL) {
        validObject(object)
        assert(
            hasClusters(object),
            isCharacter(genes),
            isFlag(perSample),
            isNumber(colMin),
            isNumber(colMax),
            isNumber(dotMin),
            isNumber(dotScale),
            isGgscale(
                x = color,
                scale = "continuous",
                aes = "color",
                nullOk = TRUE
            ),
            isFlag(legend),
            isString(title, nullOk = TRUE)
        )
        assay <- "logcounts"
        ## Fetch the gene expression data.
        x <- .fetchGeneData(
            object = object,
            genes = genes,
            assay = assay,
            metadata = TRUE
        )
        cols <- c("geneName", "sampleName", "ident")
        assert(isSubset(c(cols, assay), colnames(x)))
        f <- .group(x[, cols])
        x <- split(x = x, f = f)
        x <- SplitDataFrameList(lapply(
            X = x,
            FUN = function(x) {
                value <- x[[assay]]
                ## Assuming use of logcounts here.
                avgExp <- mean(expm1(value))
                ## Consider making the threshold user definable.
                pctExp <- .percentAbove(value, threshold = 0L)
                DataFrame(
                    geneName = x[["geneName"]][[1L]],
                    sampleName = x[["sampleName"]][[1L]],
                    ident = x[["ident"]][[1L]],
                    avgExp = avgExp,
                    pctExp = pctExp
                )
            }
        ))
        x <- unlist(x, recursive = FALSE, use.names = FALSE)
        ## Calculate the average expression scale per gene.
        x <- split(x, f = x[["geneName"]])
        x <- SplitDataFrameList(lapply(
            X = x,
            FUN = function(x) {
                avgExpScale <- scale(x[["avgExp"]])
                avgExpScale <- .minMax(
                    x = avgExpScale,
                    max = colMax,
                    min = colMin
                )
                x[["avgExpScale"]] <- as.numeric(avgExpScale)
                x
            }
        ))
        x <- unlist(x, recursive = FALSE, use.names = FALSE)
        ## Apply our `dotMin` threshold.
        x[["pctExp"]][x[["pctExp"]] < dotMin] <- NA
        ## Plot.
        p <- ggplot(
            data = as.data.frame(x),
            mapping = aes(
                x = .data[["geneName"]],
                y = .data[["ident"]]
            )
        ) +
            geom_point(
                mapping = aes(
                    color = .data[["avgExpScale"]],
                    size = .data[["pctExp"]]
                ),
                show.legend = legend
            ) +
            scale_radius(range = c(0L, dotScale)) +
            labs(
                title = title,
                subtitle = assay,
                x = "gene",
                y = "cluster"
            )
        ## Handling step for multiple samples, if desired.
        if (
            isTRUE(perSample) &&
                isTRUE(hasMultipleSamples(object))
        ) {
            p <- p + facet_wrap(facets = vars(.data[["sampleName"]]))
        }
        ## Color.
        if (is(color, "ScaleContinuous")) {
            p <- p + color
        }
        ## Return.
        p
    }

formals(`plotDots,SCE`)[c("color", "legend")] <- # nolint
    .formalsList[c("continuousColorPurpleOrange", "legend")]



## Updated 2019-09-03.
`plotViolin,SCE` <- # nolint
    function(object,
             genes,
             assay = c("logcounts", "normcounts"),
             perSample = TRUE,
             scale = c("count", "width", "area"),
             color,
             legend,
             title = NULL) {
        validObject(object)
        assert(
            isCharacter(genes),
            isFlag(perSample),
            isGgscale(color, scale = "discrete", aes = "color", nullOk = TRUE),
            isFlag(legend),
            isString(title, nullOk = TRUE)
        )
        assay <- match.arg(assay)
        scale <- match.arg(scale)
        ## Fetch the gene expression data.
        data <- .fetchGeneData(
            object = object,
            genes = genes,
            assay = assay,
            metadata = TRUE
        )
        ## Handling step for multiple samples, if desired.
        if (
            isTRUE(perSample) &&
                isTRUE(hasMultipleSamples(object))
        ) {
            x <- "sampleName"
            interestingGroups <- interestingGroups(object)
            if (
                is.null(interestingGroups) ||
                    interestingGroups == "ident"
            ) {
                interestingGroups <- "sampleName"
            }
            colorMapping <- "interestingGroups"
            colorLabs <- paste(interestingGroups, collapse = ":\n")
        } else {
            x <- "ident"
            colorMapping <- x
            colorLabs <- "cluster"
        }
        ## Plot.
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[[x]],
                y = .data[[assay]],
                color = .data[[colorMapping]]
            )
        ) +
            geom_jitter(show.legend = legend) +
            geom_violin(
                fill = NA,
                scale = scale,
                adjust = 1L,
                show.legend = legend,
                trim = TRUE
            ) +
            ## Note that `scales = free_y` will hide the x-axis for some plots.
            labs(
                x = NULL,
                color = colorLabs,
                title = title
            )
        ## Handling step for multiple samples, if desired.
        if (
            isTRUE(perSample) &&
                isTRUE(hasMultipleSamples(object))
        ) {
            p <- p +
                facet_grid(
                    rows = vars(.data[["ident"]]),
                    cols = vars(.data[["geneName"]]),
                    scales = "free_y"
                )
        } else {
            p <- p +
                facet_wrap(
                    facets = vars(.data[["geneName"]]),
                    scales = "free_y"
                )
        }
        ## Color.
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
        ## Return.
        p
    }

formals(`plotViolin,SCE`)[c("color", "legend")] <- # nolint
    .formalsList[c("discreteColor", "legend")]



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotCounts,SCE`
)

#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotCounts,SE`
)

#' @rdname plotCounts
#' @export
setMethod(
    f = "plotDots",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotDots,SCE`
)

#' @rdname plotCounts
#' @export
setMethod(
    f = "plotViolin",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotViolin,SCE`
)
