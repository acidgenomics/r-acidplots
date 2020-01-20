#' @name plotCountsCorrelation
#' @inherit acidgenerics::plotCountsCorrelation
#' @note Updated 2019-09-16.
#'
#' @inheritParams base::Extract
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#'
#' ## matrix ====
#' x <- SummarizedExperiment::assay(RangedSummarizedExperiment)
#' x <- x[seq_len(4L), seq_len(2L)]
#' y <- x * 2L
#' plotCountsCorrelation(x, y)
NULL



#' @rdname plotCountsCorrelation
#' @name plotCountsCorrelation
#' @importFrom acidgenerics plotCountsCorrelation
#' @usage plotCountsCorrelation(x, y, ...)
#' @export
NULL



## Updated 2019-09-16.
`plotCountsCorrelation,matrix` <-  # nolint
    function(
        x,
        y,
        i = NULL,
        j = NULL,
        color,
        labels = list(
            title = NULL,
            subtitle = NULL,
            color = NULL,
            x = NULL,
            y = "counts"
        ),
        .xname = getNameInParent(x),
        .yname = getNameInParent(y)
    ) {
        validObject(x)
        validObject(y)
        assert(
            identical(dimnames(x), dimnames(y)),
            !anyNA(x),
            !anyNA(y),
            isString(.xname),
            isString(.yname)
        )
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
        if (!is.null(i)) {
            x <- x[i, , drop = FALSE]
            y <- y[i, , drop = FALSE]
        }
        if (!is.null(j)) {
            x <- x[, j, drop = FALSE]
            y <- y[, j, drop = FALSE]
        }
        ## Currently allowing hard maximum number of genes to plot.
        assert(
            nrow(x) > 0L && nrow(x) <= 10L,
            ncol(x) >= 2L
        )
        xData <- melt(x)
        xData[["type"]] <- factor(.xname)
        yData <- melt(y)
        yData[["type"]] <- factor(.yname)
        data <- rbind(xData, yData)
        p <- ggplot(
            data = as_tibble(data, rownames = NULL),
            mapping = aes(
                x = !!sym("colname"),
                y = !!sym("value"),
                color = !!sym("type")
            )
        ) +
            geom_point() +
            facet_wrap(facets = sym("rowname"), scales = "free_y")
        ## Color.
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
        ## Labels
        if (is.list(labels)) {
            p <- p + do.call(what = labs, args = labels)
        }
        ## Return.
        p
    }

formals(`plotCountsCorrelation,matrix`)[["color"]] <-
    formalsList[["color.discrete"]]



#' @rdname plotCountsCorrelation
#' @export
setMethod(
    f = "plotCountsCorrelation",
    signature = signature(
        x = "matrix",
        y =  "matrix"
    ),
    definition = `plotCountsCorrelation,matrix`
)
