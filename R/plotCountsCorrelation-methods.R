#' @name plotCountsCorrelation
#' @inherit AcidGenerics::plotCountsCorrelation
#' @note Updated 2023-10-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## matrix ====
#' x <- SummarizedExperiment::assay(RangedSummarizedExperiment)
#' x <- x[seq_len(4L), seq_len(2L)]
#' y <- x * 2L
#' plotCountsCorrelation(x, y)
NULL



## Updated 2023-10-03.
`plotCountsCorrelation,matrix` <- # nolint
    function(x,
             y,
             i = NULL,
             j = NULL,
             labels = list(
                 "title" = NULL,
                 "subtitle" = NULL,
                 "color" = NULL,
                 "x" = NULL,
                 "y" = "counts"
             )) {
        assert(
            validObject(x),
            validObject(y),
            identical(dimnames(x), dimnames(y)),
            !anyNA(x),
            !anyNA(y)
        )
        labels <- matchLabels(labels)
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
        xData[["type"]] <- "x"
        yData <- melt(y)
        yData[["type"]] <- "y"
        data <- rbind(xData, yData)
        data[["type"]] <- as.factor(data[["type"]])
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[["colname"]],
                y = .data[["value"]],
                color = .data[["type"]]
            )
        ) +
            geom_point() +
            facet_wrap(facets = vars(.data[["rowname"]]), scales = "free_y")
        ## Color palette.
        p <- p + acid_scale_color_discrete()
        ## Labels
        p <- p + do.call(what = labs, args = labels)
        ## Return.
        p
    }



#' @rdname plotCountsCorrelation
#' @export
setMethod(
    f = "plotCountsCorrelation",
    signature = signature(
        x = "matrix",
        y = "matrix"
    ),
    definition = `plotCountsCorrelation,matrix`
)
