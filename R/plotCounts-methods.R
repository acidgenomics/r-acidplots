#' @name plotCounts
#' @inherit acidgenerics::plotCounts
#' @note Updated 2020-08-28.
#'
#' @inheritParams acidroxygen::params
#' @param genes `character` or `missing`. Gene identifiers. The function will
#'   automatically match identifiers corresponding to the rownames of the
#'   object, or gene symbols defined in the object.
#' @param line `character(1)`.
#'   Include average (median, mean, or geometric mean) line for each interesting
#'   group. Disabled by default and if samples are colored by sample name.
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
#' @importFrom acidgenerics plotCounts
#' @usage plotCounts(object, ...)
#' @export
NULL



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



## Updated 2019-08-28.
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



## Updated 2020-08-28.
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



#' Calculate standard error of the mean
#'
#' @note Updated 2020-08-31.
#' @noRd
#'
#' @details
#' Alternatively, can use: `sd(x) / sqrt(length(x))`.
#'
#' @seealso
#' - https://stackoverflow.com/questions/2676554/
.se <- function(x) {
    sqrt(var(x) / length(x))
}



## Useful posts regarding error bars:
## - https://stackoverflow.com/a/32091916/3911732
## - http://environmentalcomputing.net/
##       plotting-with-ggplot-bar-plots-with-error-bars/
##
## Updated 2020-08-28.
`plotCounts,SummarizedExperiment` <-  # nolint
    function(
        object,
        genes,
        assay = 1L,
        interestingGroups = NULL,
        convertGenesToSymbols = TRUE,
        geom = c("point", "violin", "boxplot", "bar"),
        trans = c("identity", "log2", "log10"),
        line = c("none", "median", "mean", "geometricMean"),
        color,
        fill,
        legend,
        style = c("facet", "wide"),
        labels = list(
            title = NULL,
            subtitle = NULL,
            sampleAxis = NULL,
            countAxis = "counts"
        )
    ) {
        validObject(object)
        if (missing(genes)) {
            genes <- rownames(object)
        }
        assert(
            isCharacter(genes),
            ## Limit the number of genes that can be plotted at once.
            all(isInClosedRange(length(genes), lower = 1L, upper = 20L)),
            isScalar(assay),
            isFlag(convertGenesToSymbols),
            isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE),
            isFlag(legend)
        )
        geom <- match.arg(geom)
        trans <- match.arg(trans)
        line <- match.arg(line)
        style <- match.arg(style)
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
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
        ## Generate a melted tibble.
        data <- melt(
            object = object,
            min = -Inf,
            minMethod = "absolute",
            trans = trans
        )
        ## Plot.
        p <- do.call(
            what = switch(
                EXPR = style,
                "facet" = .plotCountsFacet,
                "wide" = .plotCountsWide
            ),
            args = list(data = as_tibble(data, rownames = NULL))
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
                    fun.min = function(x) mean(x) - .se(x),
                    fun.max = function(x) mean(x) + .se(x),
                    fun = mean,
                    geom = "errorbar",
                    color = "black",
                    width = 0.15
                )
        )
        ## Average (mean/median) line.
        if (
            !identical(line, "none") &&
            !identical(interestingGroups, "sampleName")
        ) {
            cli_alert_info(sprintf("Line denotes {.fun %s}.", line))
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
        ## Color or fill.
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }
        ## Labels.
        if (is.list(labels)) {
            if (!identical(trans, "identity")) {
                labels[["countAxis"]] <- paste(trans, labels[["countAxis"]])
            }
            labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
            labels[["fill"]] <- labels[["color"]]
            names(labels)[names(labels) == "sampleAxis"] <- "x"
            names(labels)[names(labels) == "countAxis"] <- "y"
            p <- p + do.call(what = labs, args = labels)
        }
        ## Return.
        p
    }

formals(`plotCounts,SummarizedExperiment`)[["color"]] <-
    formalsList[["color.discrete"]]
formals(`plotCounts,SummarizedExperiment`)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(`plotCounts,SummarizedExperiment`)[["legend"]] <-
    formalsList[["legend"]]



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("SummarizedExperiment"),
    definition = `plotCounts,SummarizedExperiment`
)
