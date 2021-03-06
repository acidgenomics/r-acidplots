#' @name plotZerosVsDepth
#' @inherit AcidGenerics::plotZerosVsDepth
#' @note Updated 2019-12-09.
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
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotZerosVsDepth(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotZerosVsDepth(object)
NULL



## Updated 2019-12-09.
`plotZerosVsDepth,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        color,
        labels = list(
            title = "Zeros vs. depth",
            subtitle = NULL,
            x = "library size (depth)",
            y = "dropout rate"
        )
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE)
        )
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
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
        if (is.list(labels)) {
            labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
            p <- p + do.call(what = labs, args = labels)
        }
        ## Color.
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
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

formals(`plotZerosVsDepth,SummarizedExperiment`)[["color"]] <-
    formalsList[["color.discrete"]]



#' @rdname plotZerosVsDepth
#' @export
setMethod(
    f = "plotZerosVsDepth",
    signature = signature("SummarizedExperiment"),
    definition = `plotZerosVsDepth,SummarizedExperiment`
)
