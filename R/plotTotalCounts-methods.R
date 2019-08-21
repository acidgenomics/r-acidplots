#' @name plotTotalCounts
#' @inherit bioverbs::plotTotalCounts
#' @note Updated 2019-08-21.
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



## Updated 2019-08-21.
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
        metricCol <- "totalCounts"
        counts <- assay(object, i = assay)
        if (is(counts, "sparseMatrix")) {
            colSums <- Matrix::colSums
        }
        data <- sampleData(object)
        data[[metricCol]] <- colSums(counts)
        yLab <- "counts"
        if (isTRUE(perMillion)) {
            data[[metricCol]] <- data[[metricCol]] / 1e6L
            yLab <- paste(yLab, "per million")
        }
        ## Plot.
        data <- as_tibble(data, rownames = NULL)
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("sampleName"),
                y = !!sym(metricCol),
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
        ## Fill.
        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }
        ## Flip.
        if (isTRUE(flip)) {
            p <- acid_coord_flip(p)
        }
        ## Hide sample name legend.
        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(fill = FALSE)
        }
        ## Return.
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



## Updated 2019-08-21.
`plotTotalCounts,SingleCellExperiment` <-  # nolint
    function(object) {
        object <- aggregateCellsToSamples(object)
        do.call(
            what = plotTotalCounts,
            args = matchArgsToDoCall(
                args = list(object = object)
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
