#' @name plotCorrelation
#' @inherit AcidGenerics::plotCorrelation
#' @note Updated 2021-02-09.
#'
#' @inheritParams AcidRoxygen::params
#' @param xCol,yCol `character(1)` or `integer(1)`.
#'   X and Y column name or position.
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotCorrelation(object, xCol = 1L, yCol = 2L)
NULL



## FIXME NEED TO ADDRESS THESE: ..eq.label.. ..rr.label..



## Updated 2021-02-09.
`plotCorrelation,matrix` <-  # nolint
    function(
        object,
        xCol,
        yCol,
        label = FALSE,
        labels = NULL,
        trans = c("identity", "log2", "log10")
    ) {
        requireNamespaces("ggpmisc")
        validObject(object)
        assert(
            hasColnames(object),
            hasRows(object),
            isString(xCol) || isInt(xCol),
            isString(yCol) || isInt(yCol),
            isFlag(label)
        )
        trans <- match.arg(trans)
        isLog <- !identical(trans, "identity")
        object <- as.data.frame(object[, c(xCol, yCol)])
        labels <- matchLabels(labels)
        if (is.null(labels[["x"]])) {
            labels[["x"]] <- colnames(object)[[1L]]
        }
        if (is.null(labels[["y"]])) {
            labels[["y"]] <- colnames(object)[[2L]]
        }
        labs <- do.call(what = labs, args = labels)
        assert(is(labs, "labels"))
        data <- tibble("x" = object[[xCol]], "y" = object[[yCol]])
        if (isTRUE(isLog)) {
            assert(
                allArePositive(data[["x"]]),
                allArePositive(data[["y"]])
            )
        }
        if (isTRUE(label)) {
            assert(
                hasRownames(object),
                nrow(object) <= 50L
            )
            data[["label"]] <- rownames(object)
        }
        data <- data[complete.cases(data), ]
        assert(hasRows(data))
        if (isTRUE(isLog)) {
            base <- switch(EXPR = trans, "log2" = 2L, "log10" = 10L)
            limits <- list(
                "x" = c(
                    base ^ min(floor(log(data[["x"]], base = base))),
                    base ^ max(ceiling(log(data[["x"]], base = base)))
                ),
                "y" = c(
                    base ^ min(floor(log(data[["y"]], base = base))),
                    base ^ max(ceiling(log(data[["y"]], base = base)))
                )
            )
            assert(!any(unlist(limits) == 0L))
            breaks <- list(
                "x" = base ^ seq(
                    from = log(limits[["x"]][[1L]], base = base),
                    to = log(limits[["x"]][[2L]], base = base),
                    by = 1L
                ),
                "y" = base ^ seq(
                    from = log(limits[["y"]][[1L]], base = base),
                    to = log(limits[["y"]][[2L]], base = base),
                    by = 1L
                )
            )
        }
        args <- list("x" = sym("x"), "y" = sym("y"))
        if (isTRUE(label)) {
            args[["label"]] <- sym("label")
        }
        mapping <- do.call(what = aes, args = args)
        formula <- y ~ x
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
            labs
        if (isTRUE(isLog)) {
            p <- p +
                scale_x_continuous(
                    trans = trans,
                    breaks = breaks[["x"]],
                    limits = limits[["x"]],
                    labels = comma
                ) +
                scale_y_continuous(
                    trans = trans,
                    breaks = breaks[["y"]],
                    limits = limits[["y"]],
                    labels = comma
                ) +
                annotation_logticks(
                    base = base,
                    sides = "bl"
                )
        }
        if (isTRUE(label)) {
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



#' @rdname plotCorrelation
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
        validObject(object)
        assert(isString(assay) || isInt(assay))
        plotCorrelation(object = assay(x = object, i = assay), ...)
    }



#' @rdname plotCorrelation
#' @export
setMethod(
    f = "plotCorrelation",
    signature = signature("SummarizedExperiment"),
    definition = `plotCorrelation,SE`
)
