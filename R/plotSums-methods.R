#' @name plotSums
#' @inherit bioverbs::plotSums
#' @note Updated 2019-08-12.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams base::apply
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#'
#' ## matrix ====
#' object <- counts(RangedSummarizedExperiment)
#' plotSums(object, MARGIN = 1L)
#'
#' ## Matrix ====
#' object <- counts(SingleCellExperiment)
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



#' @rdname plotSums
#' @name plotSums
#' @importFrom bioverbs plotSums
#' @usage plotSums(object, ...)
#' @export
NULL



## Updated 2019-08-12.
`plotSums,matrix` <-  # nolint
    function(object, MARGIN = 1L) {
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
        data <- tibble(x = sums)
        ggplot(
            data = data,
            mapping = aes(x = !!sym("x"))
        ) +
            stat_ecdf(size = 1L) +
            scale_x_continuous(trans = "sqrt") +
            labs(x = makeLabel(fname), y = "ECDF")
    }



#' @rdname plotSums
#' @export
setMethod(
    f = "plotSums",
    signature = signature("matrix"),
    definition = `plotSums,matrix`
)



## Updated 2019-08-12.
`plotSums,Matrix` <-  # nolint
    `plotSums,matrix`



#' @rdname plotSums
#' @export
setMethod(
    f = "plotSums",
    signature = signature("Matrix"),
    definition = `plotSums,Matrix`
)



## Updated 2019-08-12.
`plotSums,SummarizedExperiment` <-  # nolint
    function(object, assay = 1L, MARGIN) {
        plotSums(
            object = assay(object, i = assay),
            MARGIN = MARGIN
        )
    }



#' @rdname plotSums
#' @export
setMethod(
    f = "plotSums",
    signature = signature("SummarizedExperiment"),
    definition = `plotSums,SummarizedExperiment`
)
