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
## Updated 2020-09-02.
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
        color,
        fill,
        legend,
        style = c("facet", "wide"),
        sort = FALSE,
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
            all(isInClosedRange(length(genes), lower = 1L, upper = 20L)),
            isScalar(assay),
            isFlag(convertGenesToSymbols),
            isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE),
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
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }
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
        p
    }

formals(`plotCounts,SE`)[["color"]] <-
    formalsList[["color.discrete"]]
formals(`plotCounts,SE`)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(`plotCounts,SE`)[["legend"]] <-
    formalsList[["legend"]]



#' @rdname plotCounts
#' @export
setMethod(
    f = "plotCounts",
    signature = signature("SummarizedExperiment"),
    definition = `plotCounts,SE`
)



## FIXME Need to harden against SCE input here.
