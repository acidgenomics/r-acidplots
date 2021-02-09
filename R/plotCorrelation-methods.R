#' @name plotCorrelation
#' @inherit AcidGenerics::plotCorrelation
#' @note Updated 2021-02-09.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotCorrelationHeatmap(object)
NULL



## FIXME RENAME X, Y TO XCOL, YCOL?
## FIXME REWORK THIS METHOD AS MATRIX.

## Updated 2021-02-09.
`plotCorrelation,matrix` <-  # nolint
    function(
        object,
        x, y,
        formula = y ~ x,
        label = FALSE,
        title = NULL,
        trans = c("identity", "log2", "log10")
    ) {
        validObject(object)
        assert(
            hasColnames(object),
            hasRows(object),
            isString(x),
            isString(y),
            isSubset(c(x, y), colnames(object)),
            is(formula, "formula"),
            isFlag(label),
            isString(title, nullOK = TRUE)
        )
        trans <- match.arg(trans)
        data <- as.data.frame(object)
        if (!identical(trans, "identity")) {
            xy <- data[, c(x, y)]
            xy <- xy[complete.cases(xy), , drop = FALSE]
            base <- switch(EXPR = trans, "log2" = 2L, "log10" = 10L)
            x_limits <- c(
                base ^ min(floor(log(na.omit(xy[[x]]), base = base))),
                base ^ max(ceiling(log(na.omit(xy[[x]]), base = base)))
            )
            y_limits <- c(
                base ^ min(floor(log(na.omit(xy[[y]]), base = base))),
                base ^ max(ceiling(log(na.omit(xy[[y]]), base = base)))
            )
            x_breaks <- base ^ seq(
                from = log(x_limits[[1L]], base = base),
                to = log(x_limits[[2L]], base = base),
                by = 1L
            )
            y_breaks <- base ^ seq(
                from = log(y_limits[[1L]], base = base),
                to = log(y_limits[[2L]], base = base),
                by = 1L
            )
        }
        ## FIXME How to make this more efficient?
        if (!is.null(label)) {
            mapping = aes(
                x = !!sym(x),
                y = !!sym(y),
                label = !!sym(label)
            )
        } else {
            mapping = aes(
                x = !!sym(x),
                y = !!sym(y)
            )
        }
        p <- ggplot(data = data, mapping = mapping) +
            geom_point() +
            geom_smooth(
                method = "lm",
                se = FALSE,
                color = "black",
                formula = formula
            ) +
            ggpmisc::stat_poly_eq(
                formula = formula,
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                parse = TRUE
            ) +
            labs(
                title = title,
                x = makeLabel(x),
                y = makeLabel(y)
            )
        if (!identical(trans, "identity")) {
            p <- p +
                scale_x_continuous(
                    trans = trans,
                    breaks = x_breaks,
                    limits = x_limits,
                    labels = comma
                ) +
                scale_y_continuous(
                    trans = trans,
                    breaks = y_breaks,
                    limits = y_limits,
                    labels = comma
                ) +
                annotation_logticks(
                    base = base,
                    sides = "bl"
                )
        }
        if (!is.null(label)) {
            p <- p + acid_geom_label_repel()
        }
        p
    }



#' @rdname plotCorrelation
#' @export
setMethod(
    f = "plotCorrelation",
    signature = signature("matrix"),
    definition = `plotCorrelation,matrix`
)



## Updated 2021-02-09.
`plotCorrelation,Matrix` <-  # nolint
    `plotCorrelation,matrix`



#' @rdname plotCorrelation
#' @export
setMethod(
    f = "plotCorrelation",
    signature = signature("Matrix"),
    definition = `plotCorrelation,Matrix`
)



## Updated 2021-02-09.
`plotCorrelation,data.frame` <-  # nolint
    `plotCorrelation,matrix`



#' @rdname plotCorrelation
#' @export
setMethod(
    f = "plotCorrelation",
    signature = signature("data.frame"),
    definition = `plotCorrelation,data.frame`
)



## Updated 2021-02-09.
`plotCorrelation,DataFrame` <-  # nolint
    `plotCorrelation,data.frame`



#' @rdname plotWaterfall
#' @export
setMethod(
    f = "plotCorrelation",
    signature = signature("DataFrame"),
    definition = `plotCorrelation,DataFrame`
)



## Updated 2021-02-09.
`plotCorrelation,SE` <-  # nolint
    function(
        object,
        assay = 1L,
        ...
    ) {
        assay <- assay(object, i = assay)
        plotCorrelation(object = assay, ...)
    }



#' @rdname plotWaterfall
#' @export
setMethod(
    f = "plotCorrelation",
    signature = signature("SummarizedExperiment"),
    definition = `plotCorrelation,SE`
)
