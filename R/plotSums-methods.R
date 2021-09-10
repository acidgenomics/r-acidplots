## FIXME Rework this to work directly on SummarizedExperiment...



#' @name plotSums
#' @inherit AcidGenerics::plotSums
#' @note Updated 2021-09-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param f `factor` or `NULL`.
#'   Interesting group factor mappings corresponding to samples defined in
#'   the columns. Used to split plot into groupings.
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
#' )
#'
#' ## matrix ====
#' object <- basejump::counts(RangedSummarizedExperiment)
#' plotSums(object, MARGIN = 1L)
#'
#' ## Matrix ====
#' object <- basejump::counts(SingleCellExperiment)
#' plotSums(object, MARGIN = 2L)
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotSums(object, MARGIN = 1L)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotSums(object, MARGIN = 1L)
NULL



## Updated 2021-09-10.
`plotSums,matrix` <-  # nolint
    function(
        object,
        MARGIN,
        f = NULL,
        labels = list(
            "title" = NULL,
            "subtitle" = NULL
        )
    ) {
        assert(
            isInt(MARGIN),
            is.factor(f) || is.null(f)
        )
        labels <- matchLabels(labels)
        fname <- switch(
            EXPR = as.character(MARGIN),
            "1" = "rowSums",
            "2" = "colSums",
            stop("Invalid MARGIN.")  # nocov
        )
        ## Providing method support here for sparse matrix.
        whatPkg <- ifelse(
            test = is(object, "Matrix"),
            yes = "Matrix",
            no = "base"
        )
        fun <- get(x = fname, envir = asNamespace(whatPkg), inherits = FALSE)
        assert(is.function(fun))
        if (is.factor(f)) {
            assert(identical(names(f), colnames(object)))
            data <- do.call(
                what = rbind,
                args = lapply(
                    X = levels(f),
                    object = object,
                    FUN = function(level, object) {
                        sums <- fun(object[, which(f == level), drop = FALSE])
                        data.frame(
                            "sample" = level,
                            "value" = unname(sums)
                        )
                    }
                )
            )
        } else {
            sums <- fun(object)
            data <- data.frame(
                "sample" = "unknown",
                "value" = unname(sums)
            )
        }
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("value"),
                color = !!sym("sample")
            )
        ) +
            stat_ecdf(size = 1L) +
            scale_x_continuous(trans = "sqrt")
        ## Labels.
        labels[["color"]] <- ""
        labels[["x"]] <- fname
        labels[["y"]] <- "Fn(x)"
        p <- p + do.call(what = labs, args = labels)
        ## Color palette.
        p <- p + autoDiscreteColorScale()
        p
    }



## Updated 2019-08-12.
`plotSums,Matrix` <-  # nolint
    `plotSums,matrix`



## Updated 2021-09-10.
`plotSums,SE` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        ...
    ) {
        validObject(object)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        metrics <- metrics(object = object, return = "DataFrame")
        f <- metrics[["interestingGroups"]]
        assert(is.factor(f))
        names(f) <- rownames(metrics)
        assay <- assay(object, i = assay)
        assert(identical(names(f), colnames(assay)))
        plotSums(object = assay, f = f, ...)
    }



#' @rdname plotSums
#' @export
setMethod(
    f = "plotSums",
    signature = signature("Matrix"),
    definition = `plotSums,Matrix`
)

#' @rdname plotSums
#' @export
setMethod(
    f = "plotSums",
    signature = signature("SummarizedExperiment"),
    definition = `plotSums,SE`
)

#' @rdname plotSums
#' @export
setMethod(
    f = "plotSums",
    signature = signature("matrix"),
    definition = `plotSums,matrix`
)
