#' @name plotCountsCorrelation
#' @inherit bioverbs::plotCountsCorrelation
#' @note Updated 2019-09-14.
#'
#' @inheritParams base::Extract
#' @inheritParams acidroxygen::params
#' @param xTitle `character(1)`.
#'   Name of object defined in `x`.
#' @param yTitle `character(1)`.
#'   Name of object defined in `y`.
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
#' @importFrom bioverbs plotCountsCorrelation
#' @usage plotCountsCorrelation(x, y, ...)
#' @export
NULL



## Updated 2019-09-14.
`plotCountsCorrelation,matrix` <-  # nolint
    function(
        x,
        y,
        i = NULL,
        j = NULL,
        color,
        xTitle = getNameInParent(x),
        yTitle = getNameInParent(y),
        labs = list(
            color = NULL,
            x = NULL,
            y = NULL,
            title = NULL,
            subtitle = NULL
        )
    ) {
        validObject(x)
        validObject(y)
        assert(
            identical(dimnames(x), dimnames(y)),
            !anyNA(x),
            !anyNA(y),
            isString(xTitle),
            isString(yTitle),
            is.list(labs)
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
        xData <- melt(x, min = NULL)
        xData[["type"]] <- factor(xTitle)
        yData <- melt(y, min = NULL)
        yData[["type"]] <- factor(yTitle)
        data <- rbind(xData, yData)
        p <- ggplot(
            data = as_tibble(data, rownames = NULL),
            mapping = aes(
                x = !!sym("colname"),
                y = !!sym("value"),
                colour = !!sym("type")
            )
        ) +
            geom_point() +
            facet_wrap(facets = sym("rowname"), scales = "free_y") +
            do.call(what = ggplot2::labs, args = labs)
        ## Color.
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
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
