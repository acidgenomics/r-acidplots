#' @name plotCounts
#' @inherit bioverbs::plotCounts
#' @note Updated 2019-11-07.
#'
#' @inheritParams acidroxygen::params
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
#' @importFrom bioverbs plotCounts
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



## Updated 2019-09-15.
.plotCountsFacet <- function(data) {
    ggplot(
        data = data,
        mapping = aes(
            x = !!sym("interestingGroups"),
            y = !!sym("value"),
            color = !!sym("interestingGroups")
        )
    ) +
        facet_wrap(facets = sym("rowname"), scales = "free_y")
}



## Updated 2019-09-15.
.plotCountsWide <- function(data) {
    ggplot(
        data = data,
        mapping = aes(
            x = !!sym("rowname"),
            y = !!sym("value"),
            color = !!sym("interestingGroups")
        )
    )
}



## Updated 2019-11-07.
`plotCounts,SummarizedExperiment` <-  # nolint
    function(
        object,
        genes,
        assay = 1L,
        interestingGroups = NULL,
        trans = c("identity", "log2", "log10"),
        ## FIXME Rework to line mode.
        ## FIXME Enable the user to disable the points in line mode.
        line = c("none", "median", "mean", "geometricMean"),
        color,
        legend,
        style = c("facet", "wide"),
        labels = list(
            title = NULL,
            subtitle = NULL,
            sampleAxis = NULL,
            countAxis = "counts"
        ),
        ...
    ) {
        ## nocov start
        call <- match.call()
        ## medianLine
        if ("medianLine" %in% names(call)) {
            stop("'medianLine' is defunct. Use 'line = \"median\"' instead.")
        }
        ## Error on unsupported arguments.
        assert(isSubset(
            x = setdiff(names(call), ""),
            y = names(formals())
        ))
        rm(call)
        ## nocov end

        validObject(object)
        assert(
            isCharacter(genes),
            ## Limit the number of genes that can be plotted at once.
            all(isInClosedRange(length(genes), lower = 1L, upper = 20L)),
            isScalar(assay),
            isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE),
            isFlag(legend)
        )
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
        p <- p + .genePoint(show.legend = legend)
        ## Average (mean/median) line.
        if (
            !identical(line, "none") &&
            !identical(interestingGroups, "sampleName")
        ) {
            message("Line denotes '", line, "()'.")
            lineFun <- get(x = line, inherits = TRUE)
            assert(is.function(lineFun))
            p <- p + stat_summary(
                fun.y = lineFun,
                fun.ymin = lineFun,
                fun.ymax = lineFun,
                geom = "crossbar",
                show.legend = FALSE,
                width = 0.5
            )
        }
        ## Color.
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
        ## Labels.
        if (is.list(labels)) {
            if (!identical(trans, "identity")) {
                labels[["countAxis"]] <- paste(trans, labels[["countAxis"]])
            }
            labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
            names(labels)[names(labels) == "sampleAxis"] <- "x"
            names(labels)[names(labels) == "countAxis"] <- "y"
            p <- p + do.call(what = labs, args = labels)
        }
        ## Return.
        p
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
