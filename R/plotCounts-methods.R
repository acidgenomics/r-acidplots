## FIXME SingleCellExperiment method is currently defined in pointillism.



#' @name plotCounts
#' @inherit AcidGenerics::plotCounts
#' @note Updated 2021-02-08.
#'
#' @inheritParams AcidRoxygen::params
#' @param genes `character` or `missing`. Gene identifiers. The function will
#'   automatically match identifiers corresponding to the rownames of the
#'   object, or gene symbols defined in the object.
#' @param line `character(1)`.
#'   Include average (median, mean, or geometric mean) line for each interesting
#'   group. Disabled by default and if samples are colored by sample name.
#' @param style `character(1)`.
#'   Plot style.
#' @param sort `logical(1)`.
#'   Sort the genes alphabetically.
#'   This setting applies to the gene symbols rather than the gene identifiers
#'   when `convertGenesToSymbols` is `TRUE`.
#' @param ... Additional arguments.
#'
#' @return
#' - `style = "facet"`: `ggplot` grouped by `sampleName`, with
#'   `ggplot2::facet_wrap()` applied to panel the samples.
#' - `style = "wide"`: `ggplot` in wide format, with genes on the x-axis.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' rownames <- head(rownames(object))
#' print(rownames)
#' g2s <- basejump::Gene2Symbol(object)
#' geneIds <- head(g2s[[1L]])
#' print(geneIds)
#' geneNames <- head(g2s[[2L]])
#' print(geneNames)
#'
#' ## Rownames, gene IDs, and gene names (symbols) are supported.
#' plotCounts(object, genes = geneIds, style = "facet")
#' plotCounts(object, genes = geneNames, style = "wide")
NULL



#' Improved gene point geom
#' @note Updated 2019-07-23.
#' @noRd
.genePoint <- function(
    size = 3L,
    alpha = 1L,
    ...
) {
    geom_point(
        ...,
        size = size,
        alpha = alpha,
        position = position_jitterdodge(dodge.width = 0.9)
    )
}



#' Facet wrap the counts plot
#' @note Updated 2019-08-28.
#' @noRd
.plotCountsFacet <- function(data) {
    ggplot(
        data = data,
        mapping = aes(
            x = str_replace_na(!!sym("interestingGroups")),
            y = !!sym("value"),
            color = str_replace_na(!!sym("interestingGroups")),
            fill = str_replace_na(!!sym("interestingGroups"))
        )
    ) +
        facet_wrap(facets = sym("rowname"), scales = "free_y")
}



#' Display the counts plot in wide format
#' @note Updated 2020-08-28.
#' @noRd
.plotCountsWide <- function(data) {
    ggplot(
        data = data,
        mapping = aes(
            x = !!sym("rowname"),
            y = !!sym("value"),
            color = str_replace_na(!!sym("interestingGroups")),
            fill = str_replace_na(!!sym("interestingGroups"))
        )
    )
}



## Coercing to `SummarizedExperiment` internally for fast subsetting.
##
## Useful posts regarding error bars:
## - https://stackoverflow.com/a/32091916/3911732
## - http://environmentalcomputing.net/
##       plotting-with-ggplot-bar-plots-with-error-bars/
##
## Updated 2021-09-10.
`plotCounts,SE` <-  # nolint
    function(
        object,
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
        )
    ) {
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
        data <- as_tibble(data, rownames = NULL)
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
            args = list(data = data)
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
        p <- p + autoDiscreteColorScale()
        p <- p + autoDiscreteFillScale()
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

formals(`plotCounts,SE`)[["legend"]] <-
    .formalsList[["legend"]]








#' @name plotCounts
#' @aliases plotDots plotViolin
#' @inherit AcidGenerics::plotCounts
#'
#' @note Dot geom currently only supports logcounts.
#' @note Updated 2020-10-12.
#'
#' @description Visualize genes on a dot or violin plot.
#'
#' @inheritParams AcidRoxygen::params
#' @param colMin `numeric(1)`.
#'   Minimum scaled average expression threshold. Everything smaller will be
#'   set to this.
#' @param colMax `numeric(1)`.
#'   Maximum scaled average expression threshold. Everything larger will be set
#'   to this.
#' @param dotMin `numeric(1)`.
#'   The fraction of cells at which to draw the smallest dot. All cell groups
#'   with less than this expressing the given gene will have no dot drawn.
#' @param dotScale `numeric(1)`.
#'   Scale the size of the points, similar to `cex`.
#' @param geom `character(1)`.
#'   Plot type. Uses [`match.arg()`][base::match.arg] to pick the type.
#'   Currently supports `"dot"` and `"violin"`.
#' @param scale `character(1)`.
#'   If "area" (default), all violins have the same area (before trimming the
#'   tails). If "count", areas are scaled proportionally to the number of
#'   observartions. If "width", all violins have the same maximum width.
#'   See `ggplot2::geom_violin` for details.

#' @param ... Additional arguments.
#'
#' @seealso
#' - `Seurat::DotPlot()`.
#' - `Seurat::VlnPlot()`.
#' - `Seurat::RidgePlot()`.
#' - `monocle3::plot_genes_violin()`.
#'
#' @examples
#' data(Seurat, package = "AcidTest")
#'
#' ## Seurat ====
#' object <- Seurat
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



## Updated 2021-09-13.
`plotCounts,SCE` <-  # nolint
    function(
        object,
        genes,
        assay = c("logcounts", "normcounts"),
        geom = c("violin", "dot"),
        perSample = TRUE,
        legend,
        title = NULL
    ) {
        validObject(object)
        assay <- match.arg(assay)
        geom <- match.arg(geom)
        args <- as.list(sys.call(which = -1L))[-1L]
        args[["geom"]] <- NULL
        if (geom == "dot") {
            assert(identical(assay, "logcounts"))
            args[["assay"]] <- NULL
            what <- plotDots
        } else if (geom == "violin") {
            what <- plotViolin
        }
        do.call(what = what, args = args)
    }

formals(`plotCounts,SCE`)[["legend"]] <-
    .formalsList[["legend"]]



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("SingleCellExperiment"),
    definition = `plotCounts,SCE`
)

#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("SummarizedExperiment"),
    definition = `plotCounts,SE`
)
