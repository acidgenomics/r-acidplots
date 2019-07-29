#' @name plotCountsCorrelation
#' @inherit bioverbs::plotCountsCorrelation
#' @note Updated 2019-07-29.
#'
#' @inheritParams base::Extract
#' @inheritParams params
#' @param xTitle `character(1)`.
#'   Name of object defined in `x`.
#' @param yTitle `character(1)`.
#'   Name of object defined in `y`.
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#' rse <- RangedSummarizedExperiment
#'
#' ## matrix ====
#' x <- SummarizedExperiment::assay(rse)
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



## Updated 2019-07-23.
`plotCountsCorrelation,matrix` <-  # nolint
    function(
        x,
        y,
        i = NULL,
        j = NULL,
        xTitle = deparse(substitute(x)),
        yTitle = deparse(substitute(y)),
        labs = list(
            colour = NULL,
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
        xData <- x %>%
            meltCounts(minCounts = NULL) %>%
            ungroup() %>%
            mutate_if(is.factor, as.character) %>%
            mutate(!!sym("type") := xTitle)
        yData <- y %>%
            meltCounts(minCounts = NULL) %>%
            ungroup() %>%
            mutate_if(is.factor, as.character) %>%
            mutate(!!sym("type") := yTitle)
        data <- bind_rows(xData, yData)
        ggplot(
            data = data,
            mapping = aes(
                x = !!sym("colname"),
                y = !!sym("counts"),
                colour = !!sym("type")
            )
        ) +
            geom_point() +
            facet_wrap(facets = sym("rowname"), scales = "free_y") +
            do.call(what = ggplot2::labs, args = labs)
    }



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
