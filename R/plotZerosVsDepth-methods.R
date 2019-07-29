#' @name plotZerosVsDepth
#' @inherit bioverbs::plotZerosVsDepth
#' @note Updated 2019-07-29.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#' rse <- RangedSummarizedExperiment
#' sce <- SingleCellExperiment
#'
#' ## SummarizedExperiment ====
#' plotZerosVsDepth(rse)
#'
#' ## SingleCellExperiment ====
#' plotZerosVsDepth(sce)
NULL



#' @rdname plotZerosVsDepth
#' @name plotZerosVsDepth
#' @importFrom bioverbs plotZerosVsDepth
#' @usage plotZerosVsDepth(object, ...)
#' @export
NULL



## Updated 2019-07-23.
`plotZerosVsDepth,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        color,
        title = "Zeros vs. depth"
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isGGScale(color, scale = "discrete", aes = "colour", nullOK = TRUE),
            isString(title, nullOK = TRUE)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)

        data <- zerosVsDepth(object, assay = assay)
        assert(is(data, "DataFrame"))

        p <- ggplot(
            data = as_tibble(data),
            mapping = aes(
                x = !!sym("depth"),
                y = !!sym("dropout"),
                color = !!sym("interestingGroups")
            )
        ) +
            geom_point(size = 0.8, alpha = 0.8) +
            expand_limits(y = c(0L, 1L)) +
            scale_x_continuous(trans = "log10") +
            labs(
                title = title,
                x = "library size (depth)",
                y = "dropout rate",
                color = paste(interestingGroups, collapse = ":\n")
            )

        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }

        ## Wrap samples by `aggregate` column, if defined.
        facets <- NULL
        if (isTRUE("aggregate" %in% colnames(data))) {
            facets <- "aggregate"
        }
        if (is.character(facets)) {
            p <- p + facet_wrap(facets = syms(facets), scales = "free")
        }

        p
    }

formals(`plotZerosVsDepth,SummarizedExperiment`)[["color"]] <-
    formalsList[["color.discrete"]]



#' @rdname plotZerosVsDepth
#' @export
setMethod(
    f = "plotZerosVsDepth",
    signature = signature("SummarizedExperiment"),
    definition = `plotZerosVsDepth,SummarizedExperiment`
)
