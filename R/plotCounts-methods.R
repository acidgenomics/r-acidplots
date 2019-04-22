#' @name plotCounts
#' @inherit bioverbs::plotCounts
#' @inheritParams params
#'
#' @param countsAxisLabel `character(1)`.
#'   Label to use for the counts axis.
#' @param medianLine `logical(1)`.
#'   Include median line for each group. Disabled if samples are colored by
#'   sample name.
#' @param style `character(1)`.
#'   Plot style.
#'
#' @return
#' - `style = "facet"`: `ggplot` grouped by `sampleName`, with
#'   [ggplot2::facet_wrap()] applied to panel the samples.
#' - `style = "wide"`: `ggplot` in wide format, with genes on the x-axis.
#'
#' @examples
#' data(rse, package = "acidtest")
#'
#' rownames <- head(rownames(rse))
#' print(rownames)
#' g2s <- basejump::Gene2Symbol(rse)
#' geneIDs <- head(g2s[["geneID"]])
#' print(geneIDs)
#' geneNames <- head(g2s[["geneName"]])
#' print(geneNames)
#'
#' ## Rownames, gene IDs, and gene names (symbols) are supported.
#' plotCounts(rse, genes = geneIDs, style = "facet")
#' plotCounts(rse, genes = geneNames, style = "wide")
NULL



#' @rdname plotCounts
#' @name plotCounts
#' @importFrom bioverbs plotCounts
#' @export
NULL



.geneMedianLine <- stat_summary(
    fun.y = median,
    fun.ymin = median,
    fun.ymax = median,
    geom = "crossbar",
    show.legend = FALSE,
    width = 0.5
)



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



.plotCountsFacet <- function(
    object,
    trans,
    countsAxisLabel,
    medianLine,
    color,
    legend
) {
    assert(is(object, "SummarizedExperiment"))
    interestingGroups <- interestingGroups(object)

    # Coerce the data to a melted tibble.
    suppressMessages(
        data <- meltCounts(object, trans = trans)
    )

    p <- ggplot(
        data = data,
        mapping = aes(
            x = !!sym("interestingGroups"),
            y = !!sym("counts"),
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

    if (
        isTRUE(medianLine) &&
        !identical(interestingGroups, "sampleName")
    ) {
        p <- p + .geneMedianLine
    }

    if (is(color, "ScaleDiscrete")) {
        p <- p + color
    }

    if (identical(interestingGroups, "sampleName")) {
        p <- p + guides(color = FALSE)
    }

    p
}



.plotCountsWide <- function(
    object,
    trans,
    countsAxisLabel,
    medianLine,
    color,
    legend
) {
    assert(is(object, "SummarizedExperiment"))
    interestingGroups <- interestingGroups(object)

    # Coerce the data to a melted tibble.
    suppressMessages(
        data <- meltCounts(object, trans = trans)
    )

    p <- ggplot(
        data = data,
        mapping = aes(
            x = !!sym("rowname"),
            y = !!sym("counts"),
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

    if (
        isTRUE(medianLine) &&
        !identical(interestingGroups, "sampleName")
    ) {
        p <- p + .geneMedianLine
    }

    if (is(color, "ScaleDiscrete")) {
        p <- p + color
    }

    p
}



plotCounts.SummarizedExperiment <-  # nolint
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
        # Detect DESeqDataSet and use normalized counts, if necessary.
        if (is(object, "DESeqDataSet")) {
            message("DESeqDataSet detected. Using normalized counts.")
            assays <- list(normalized = counts(object, normalized = TRUE))
            object <- as(object, "RangedSummarizedExperiment")
            assays(object) <- assays
            assay <- 1L
            countsAxisLabel <- "normalized counts"
        }
        validObject(object)
        assert(
            isCharacter(genes),
            # Limit the number of genes that can be plotted at once.
            all(isInClosedRange(length(genes), lower = 1L, upper = 20L)),
            isScalar(assay),
            isString(countsAxisLabel),
            isFlag(medianLine),
            isGGScale(color, scale = "discrete", aes = "colour", nullOK = TRUE),
            isFlag(legend)
        )
        trans <- match.arg(trans)
        style <- match.arg(style)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)

        # Coercing to `SummarizedExperiment` for fast subsetting below.
        object <- as.SummarizedExperiment(object)
        genes <- mapGenesToRownames(object, genes = genes, strict = FALSE)

        # Minimize the SE object only contain the assay of our choice.
        assay <- assays(object)[[assay]]
        assays(object) <- list(assay = assay)

        # Subset to match the genes, which have been mapped to the rownames.
        object <- object[genes, , drop = FALSE]
        # Now convert the rownames to symbols, for visualization.
        suppressMessages(
            object <- convertGenesToSymbols(object)
        )

        # Counts axis label. Automatically add transformation, if necessary.
        if (trans != "identity") {
            countsAxisLabel <- paste(trans, countsAxisLabel)
        }

        # Plot style.
        if (style == "facet") {
            what <- .plotCountsFacet
        } else if (style == "wide") {
            what <- .plotCountsWide
        }

        do.call(
            what = what,
            args = list(
                object = object,
                trans = trans,
                countsAxisLabel = countsAxisLabel,
                medianLine = medianLine,
                color = color,
                legend = legend
            )
        )
    }

formals(plotCounts.SummarizedExperiment)[["color"]] <-
    formalsList[["color.discrete"]]
formals(plotCounts.SummarizedExperiment)[["legend"]] <-
    formalsList[["legend"]]



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("SummarizedExperiment"),
    definition = plotCounts.SummarizedExperiment
)
