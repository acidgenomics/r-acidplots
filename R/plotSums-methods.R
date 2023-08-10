#' @name plotSums
#' @inherit AcidGenerics::plotSums
#' @note Updated 2023-08-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotSums(object, MARGIN = 1L)
#' plotSums(object, MARGIN = 2L)
NULL



## Updated 2023-04-27.
`plotSums,SE` <- # nolint
    function(object,
             MARGIN, # nolint
             assay = 1L,
             interestingGroups = NULL,
             labels = list(
                 "title" = NULL,
                 "subtitle" = NULL
             )) {
        assert(
            validObject(object),
            isInt(MARGIN),
            isScalar(assay)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        labels <- matchLabels(labels)
        fname <- switch(
            EXPR = as.character(MARGIN),
            "1" = "rowSums",
            "2" = "colSums",
            stop("Invalid MARGIN.") # nocov
        )
        ## Get the interestingGroups factor to split object.
        metrics <- metrics(object)
        f <- metrics[["interestingGroups"]]
        assert(is.factor(f))
        names(f) <- rownames(metrics)
        assay <- assay(object, i = assay)
        assert(identical(names(f), colnames(assay)))
        whatPkg <- ifelse(
            test = is(assay, "Matrix"),
            yes = "Matrix",
            no = "base"
        )
        fun <- get(x = fname, envir = asNamespace(whatPkg), inherits = FALSE)
        assert(
            is.function(fun),
            identical(names(f), colnames(object))
        )
        data <- do.call(
            what = rbind,
            args = lapply(
                X = levels(f),
                assay = assay,
                f = f,
                FUN = function(level, assay, f) {
                    idx <- which(f == level)
                    assay <- assay[, idx, drop = FALSE]
                    sums <- fun(assay)
                    data.frame(
                        "sample" = level,
                        "value" = unname(sums)
                    )
                }
            )
        )
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("value"),
                color = !!sym("sample")
            )
        ) +
            stat_ecdf(linewidth = 1L) +
            scale_x_continuous(trans = "sqrt")
        ## Labels.
        labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
        labels[["x"]] <- fname
        labels[["y"]] <- "Fn(x)"
        p <- p + do.call(what = labs, args = labels)
        ## Color palette.
        p <- p + autoDiscreteColorScale()
        p
    }



#' @rdname plotSums
#' @export
setMethod(
    f = "plotSums",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotSums,SE`
)
