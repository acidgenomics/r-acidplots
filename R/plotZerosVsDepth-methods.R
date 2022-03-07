## FIXME Inherit the default point size here.



#' @name plotZerosVsDepth
#' @inherit AcidGenerics::plotZerosVsDepth
#' @note Updated 2022-03-07.
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



## Updated 2021-09-10.
`plotZerosVsDepth,SE` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        labels = list(
            "title" = "Zeros vs. depth",
            "subtitle" = NULL,
            "x" = "library size (depth)",
            "y" = "dropout rate"
        )
    ) {
        validObject(object)
        assert(isScalar(assay))
        labels <- matchLabels(labels)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        data <- zerosVsDepth(object, assay = assay)
        data <- as_tibble(data, rownames = NULL)
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("depth"),
                y = !!sym("dropout"),
                color = str_replace_na(!!sym("interestingGroups"))
            )
        ) +
            geom_point(size = 0.8, alpha = 0.8) +
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
            p <- p + facet_wrap(facets = syms(facets), scales = "free")
        }
        ## Return.
        p
    }



#' @rdname plotZerosVsDepth
#' @export
setMethod(
    f = "plotZerosVsDepth",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotZerosVsDepth,SE`
)
