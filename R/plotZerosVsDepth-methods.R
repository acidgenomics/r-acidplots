#' @name plotZerosVsDepth
#' @inherit AcidGenerics::plotZerosVsDepth
#' @note Updated 2022-11-09.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment_splatter,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotZerosVsDepth(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' plotZerosVsDepth(object)
NULL



## Updated 2022-11-09.
`plotZerosVsDepth,SE` <- # nolint
    function(object,
             assay = 1L,
             interestingGroups = NULL,
             pointAlpha,
             pointSize,
             labels = list(
                 "title" = "Zeros vs. depth",
                 "subtitle" = NULL,
                 "x" = "library size (depth)",
                 "y" = "dropout rate"
             )) {
        validObject(object)
        assert(
            isScalar(assay),
            isAlpha(pointAlpha),
            isNumber(pointSize)
        )
        labels <- matchLabels(labels)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        data <- zerosVsDepth(object, assay = assay)
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[["depth"]],
                y = .data[["dropout"]],
                color = str_replace_na(.data[["interestingGroups"]])
            )
        ) +
            geom_point(
                alpha = pointAlpha,
                size = pointSize
            ) +
            expand_limits(y = c(0L, 1L)) +
            scale_x_continuous(trans = "log10")
        ## Labels.
        labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
        p <- p + do.call(what = labs, args = labels)
        ## Color palette.
        p <- p + autoDiscreteColorScale()
        ## Wrap samples by `aggregate` column, if defined.
        facets <- NULL
        if (isSubset("aggregate", colnames(data))) {
            facets <- "aggregate"
        }
        if (is.character(facets)) {
            ## FIXME How to rework this without syms?
            p <- p + facet_wrap(
                facets = vars(!!!syms(facets)),
                scales = "free"
            )
        }
        ## Return.
        p
    }

formals(`plotZerosVsDepth,SE`)[c( # nolint
    "pointAlpha",
    "pointSize"
)] <- .formalsList[c(
    "pointAlpha",
    "pointSize2"
)]



#' @rdname plotZerosVsDepth
#' @export
setMethod(
    f = "plotZerosVsDepth",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotZerosVsDepth,SE`
)
