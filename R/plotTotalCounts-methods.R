#' @name plotTotalCounts
#' @inherit acidgenerics::plotTotalCounts
#' @note Updated 2019-12-09.
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
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotTotalCounts(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotTotalCounts(object)
NULL



#' @rdname plotTotalCounts
#' @name plotTotalCounts
#' @importFrom acidgenerics plotTotalCounts
#' @usage plotTotalCounts(object, ...)
#' @export
NULL



## Updated 2019-12-09.
`plotTotalCounts,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        perMillion = FALSE,
        fill,
        labels = list(
            title = "Total counts",
            subtitle = NULL,
            x = NULL,
            y = "counts"
        ),
        flip
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isFlag(perMillion),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isFlag(flip)
        )
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
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
        if (isTRUE(perMillion)) {
            data[[metricCol]] <- data[[metricCol]] / 1e6L
            labels[["y"]] <- paste(labels[["y"]], "(per million)")
        }
        ## Plot.
        data <- as_tibble(data, rownames = NULL)
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("sampleName"),
                y = !!sym(metricCol),
                fill = str_replace_na(!!sym("interestingGroups"))
            )
        ) +
            acid_geom_bar() +
            acid_scale_y_continuous_nopad()
        ## Labels.
        if (is.list(labels)) {
            labels[["fill"]] <- paste(interestingGroups, collapse = ":\n")
            p <- p + do.call(what = labs, args = labels)
        }
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

f <- formals(`plotTotalCounts,SummarizedExperiment`)
f[["fill"]] <- formalsList[["fill.discrete"]]
f[["flip"]] <- formalsList[["flip"]]
formals(`plotTotalCounts,SummarizedExperiment`) <- f



#' @rdname plotTotalCounts
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature("SummarizedExperiment"),
    definition = `plotTotalCounts,SummarizedExperiment`
)



## Updated 2019-09-15.
`plotTotalCounts,SingleCellExperiment` <-  # nolint
    function(object, ...) {
        plotTotalCounts(
            object = aggregateCellsToSamples(object),
            ...
        )
    }



#' @describeIn plotTotalCounts Applies [aggregateCellsToSamples()] calculation
#'   to summarize at sample level prior to plotting.\cr
#'   Passes `...` to `SummarizedExperiment` method.
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature("SingleCellExperiment"),
    definition = `plotTotalCounts,SingleCellExperiment`
)
