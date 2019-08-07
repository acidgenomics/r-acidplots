#' @name plotTotalCounts
#' @inherit bioverbs::plotTotalCounts
#' @note Updated 2019-07-29.
#'
#' @inheritParams acidroxygen::params
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
#' plotTotalCounts(rse)
#'
#' ## SingleCellExperiment ====
#' plotTotalCounts(sce)
NULL



#' @rdname plotTotalCounts
#' @name plotTotalCounts
#' @importFrom bioverbs plotTotalCounts
#' @usage plotTotalCounts(object, ...)
#' @export
NULL



## Updated 2019-07-23.
`plotTotalCounts,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        perMillion = FALSE,
        fill,
        flip,
        title = "Total counts"
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isFlag(perMillion),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isFlag(flip),
            isString(title, nullOK = TRUE)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)

        counts <- assays(object)[[assay]]
        if (is(counts, "sparseMatrix")) {
            colSums <- Matrix::colSums
        }
        data <- sampleData(object) %>%
            as_tibble() %>%
            mutate(totalCounts = colSums(!!counts))

        yLab <- "counts"
        if (isTRUE(perMillion)) {
            data <- mutate(data, totalCounts = !!sym("totalCounts") / 1e6L)
            yLab <- paste(yLab, "per million")
        }

        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("sampleName"),
                y = !!sym("totalCounts"),
                fill = !!sym("interestingGroups")
            )
        ) +
            acid_geom_bar() +
            acid_scale_y_continuous_nopad() +
            labs(
                title = title,
                x = NULL,
                y = yLab,
                fill = paste(interestingGroups, collapse = ":\n")
            )

        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }

        if (isTRUE(flip)) {
            p <- acid_coord_flip(p)
        }

        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(fill = FALSE)
        }

        p
    }

formals(`plotTotalCounts,SummarizedExperiment`)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(`plotTotalCounts,SummarizedExperiment`)[["flip"]] <-
    formalsList[["flip"]]



#' @rdname plotTotalCounts
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature("SummarizedExperiment"),
    definition = `plotTotalCounts,SummarizedExperiment`
)



## Updated 2019-07-23.
`plotTotalCounts,SingleCellExperiment` <-  # nolint
    function(object) {
        do.call(
            what = plotTotalCounts,
            args = matchArgsToDoCall(
                args = list(object = aggregateCellsToSamples(object))
            )
        )
    }

formals(`plotTotalCounts,SingleCellExperiment`) <-
    formals(`plotTotalCounts,SummarizedExperiment`)



#' @rdname plotTotalCounts
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature("SingleCellExperiment"),
    definition = `plotTotalCounts,SingleCellExperiment`
)
