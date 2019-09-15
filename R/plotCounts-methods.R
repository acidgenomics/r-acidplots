#' @name plotCounts
#' @inherit bioverbs::plotCounts
#' @note Updated 2019-09-13.
#'
#' @inheritParams acidroxygen::params
#' @param countsAxisLabel `character(1)`.
#'   Label to use for the counts axis.
#' @param medianLine `logical(1)`.
#'   Include median line for each group. Disabled if samples are colored by
#'   sample name.
#' @param style `character(1)`.
#'   Plot style.
#' @param ... Additional arguments.
#'
#' @return
#' - `style = "facet"`: `ggplot` grouped by `sampleName`, with
#'   [ggplot2::facet_wrap()] applied to panel the samples.
#' - `style = "wide"`: `ggplot` in wide format, with genes on the x-axis.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' rownames <- head(rownames(object))
#' print(rownames)
#' g2s <- basejump::Gene2Symbol(object)
#' geneIDs <- head(g2s[["geneID"]])
#' print(geneIDs)
#' geneNames <- head(g2s[["geneName"]])
#' print(geneNames)
#'
#' ## Rownames, gene IDs, and gene names (symbols) are supported.
#' plotCounts(object, genes = geneIDs, style = "facet")
#' plotCounts(object, genes = geneNames, style = "wide")
NULL



#' @rdname plotCounts
#' @name plotCounts
#' @importFrom bioverbs plotCounts
#' @usage plotCounts(object, ...)
#' @export
NULL



## Updated 2019-07-23.
.geneMedianLine <- stat_summary(
    fun.y = median,
    fun.ymin = median,
    fun.ymax = median,
    geom = "crossbar",
    show.legend = FALSE,
    width = 0.5
)



## Updated 2019-07-23.
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



## Updated 2019-08-27.
.plotCountsFacet <- function(
    data,
    countsAxisLabel,
    medianLine,
    color,
    legend,
    interestingGroups
) {
    assert(is(data, "tbl_df"))
    ## Plot.
    p <- ggplot(
        data = data,
        mapping = aes(
            x = !!sym("interestingGroups"),
            y = !!sym("value"),
            color = !!sym("interestingGroups")
        )
    ) +
        .genePoint(show.legend = legend) +
        scale_y_continuous() +
        facet_wrap(facets = sym("rowname"), scales = "free_y") +
        labs(
            x = NULL,
            y = countsAxisLabel,
            color = paste(interestingGroups, collapse = ":\n")
        )
    ## Median line.
    if (
        isTRUE(medianLine) &&
        !identical(interestingGroups, "sampleName")
    ) {
        p <- p + .geneMedianLine
    }
    ## Color.
    if (is(color, "ScaleDiscrete")) {
        p <- p + color
    }
    ## Hide sample name legend.
    if (identical(interestingGroups, "sampleName")) {
        p <- p + guides(color = FALSE)
    }
    ## Return.
    p
}



## Updated 2019-07-27.
.plotCountsWide <- function(
    data,
    countsAxisLabel,
    medianLine,
    color,
    legend,
    interestingGroups
) {
    assert(is(data, "tbl_df"))
    ## Plot.
    p <- ggplot(
        data = data,
        mapping = aes(
            x = !!sym("rowname"),
            y = !!sym("value"),
            color = !!sym("interestingGroups")
        )
    ) +
        .genePoint(show.legend = legend) +
        scale_y_continuous() +
        labs(
            x = NULL,
            y = countsAxisLabel,
            color = paste(interestingGroups, collapse = ":\n")
        )
    ## Median line.
    if (
        isTRUE(medianLine) &&
        !identical(interestingGroups, "sampleName")
    ) {
        p <- p + .geneMedianLine
    }
    ## Color.
    if (is(color, "ScaleDiscrete")) {
        p <- p + color
    }
    ## Return.
    p
}



## Updated 2019-08-26.
`plotCounts,SummarizedExperiment` <-  # nolint
    function(
        object,
        genes,
        assay = 1L,
        interestingGroups = NULL,
        trans = c("identity", "log2", "log10"),
        countsAxisLabel = "counts",
        medianLine = TRUE,
        color,
        legend,
        style = c("facet", "wide")
    ) {
        validObject(object)
        assert(
            isCharacter(genes),
            ## Limit the number of genes that can be plotted at once.
            all(isInClosedRange(length(genes), lower = 1L, upper = 20L)),
            isScalar(assay),
            isString(countsAxisLabel),
            isFlag(medianLine),
            isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE),
            isFlag(legend)
        )
        trans <- match.arg(trans)
        style <- match.arg(style)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        ## Coercing to `SummarizedExperiment` for fast subsetting.
        object <- as.SummarizedExperiment(object)
        ## This will support objects that don't contain gene-to-symbol mappings.
        genes <- mapGenesToRownames(object, genes = genes, strict = FALSE)
        ## Minimize the SE object only contain the assay of our choice.
        assay <- assay(object, i = assay)
        assays(object) <- SimpleList(assay = assay)
        ## Subset to match the genes, which have been mapped to the rownames.
        object <- object[genes, , drop = FALSE]
        ## Now convert the row names to symbols, for visualization.
        object <- tryCatch(
            expr = {
                suppressMessages(
                    object <- convertGenesToSymbols(object)
                )
            },
            error = function(e) {
                object
            }
        )
        ## Counts axis label. Automatically add transformation, if necessary.
        if (!identical(trans, "identity")) {
            countsAxisLabel <- paste(trans, countsAxisLabel)
        }
        ## Generate a melted tibble.
        data <- melt(object, min = 1L, trans = trans)
        data <- as_tibble(data, rownames = NULL)
        ## Plot style.
        what <- switch(
            EXPR = style,
            "facet" = .plotCountsFacet,
            "wide" = .plotCountsWide
        )
        do.call(
            what = what,
            args = list(
                data = data,
                countsAxisLabel = countsAxisLabel,
                medianLine = medianLine,
                color = color,
                legend = legend,
                interestingGroups = interestingGroups(object)
            )
        )
    }

formals(`plotCounts,SummarizedExperiment`)[["color"]] <-
    formalsList[["color.discrete"]]
formals(`plotCounts,SummarizedExperiment`)[["legend"]] <-
    formalsList[["legend"]]



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("SummarizedExperiment"),
    definition = `plotCounts,SummarizedExperiment`
)



## Updated 2019-08-27.
`plotCounts,DESeqDataSet` <-  # nolint
    function(object, ...) {
        normalized <- counts(object, normalized = TRUE)
        object <- as(object, "RangedSummarizedExperiment")
        assays(object) <- SimpleList(normalized = normalized)
        plotCounts(
            object = object,
            assay = "normalized",
            countsAxisLabel = "normalized counts",
            ...
        )
    }



#' @describeIn plotCounts Automatically plots normalized counts. Arguments pass
#'   through to `SummarizedExperiment` method, but `assay` and `countsAxisLabel`
#'   are automatic.
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("DESeqDataSet"),
    definition = `plotCounts,DESeqDataSet`
)
