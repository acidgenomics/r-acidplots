## FIXME Need to be able to batch these by interesting group.
## FIXME How to handle when interestingGroups are defined per sample?
##       Just plot everything in this case?
##



#' @name plotSums
#' @inherit AcidGenerics::plotSums
#' @note Updated 2019-09-15.
#'
#' @inheritParams AcidRoxygen::params
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
#' object <- SingleCellExperiment
#' plotSums(object)
#'
#' ## SingleCellExperiment
#' object <- RangedSummarizedExperiment
#' plotSums(object)
NULL



## FIXME Add `f` support that is NULL by default.

## Updated 2019-09-15.
`plotSums,matrix` <-  # nolint
    function(
        object,
        MARGIN  # nolint
    ) {
        assert(isInt(MARGIN))
        fname <- switch(EXPR = MARGIN, "1" = "rowSums", "2" = "colSums")
        ## Providing method support here for sparse matrix.
        if (is(object, "Matrix")) {
            pkg <- "Matrix"
        } else {
            pkg <- "base"
        }
        fun <- get(x = fname, envir = asNamespace(pkg), inherits = FALSE)
        sums <- fun(object)
        data <- data.frame(x = sums)
        ggplot(
            data = data,
            mapping = aes(x = !!sym("x"))
        ) +
            stat_ecdf(size = 1L) +
            scale_x_continuous(trans = "sqrt") +
            labs(
                x = fname,
                y = "Fn(x)"
            )
    }



## Updated 2019-08-12.
`plotSums,Matrix` <-  # nolint
    `plotSums,matrix`



## FIXME Can we batch these by interestingGroup factor mapping?
## FIXME Need to pass this through as factor in matrix method.
## Updated 2019-08-12.
`plotSums,SE` <-  # nolint
    function(
        object,
        assay = 1L,
        MARGIN  # nolint
    ) {

        ## FIXME Need to return an `interestingGroups` factor that works with
        ## SingleCellExperiment. Can't use sampleData approach for this.
        ## FIXME Define this mapping as `f` argument.

        assay <- assay(object, i = assay)
        plotSums(
            object = assay,
            MARGIN = MARGIN
        )
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
