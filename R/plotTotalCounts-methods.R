#' @name plotTotalCounts
#' @inherit bioverbs::plotTotalCounts
#' @inheritParams params
#' @examples
#' data(rse, sce, package = "acidtest")
#' plotTotalCounts(rse)
#' plotTotalCounts(sce)
NULL



#' @importFrom bioverbs plotTotalCounts
#' @aliases NULL
#' @export
bioverbs::plotTotalCounts



plotTotalCounts.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        perMillion = FALSE,
        fill,
        flip,
        title = "total counts"
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
            geom_bar(
                color = "black",
                stat = "identity"
            ) +
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
            p <- p + coord_flip()
        }

        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(fill = FALSE)
        }

        p
    }

formals(plotTotalCounts.SummarizedExperiment)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(plotTotalCounts.SummarizedExperiment)[["flip"]] <-
    formalsList[["flip"]]



plotTotalCounts.SingleCellExperiment <-  # nolint
    function(object) {
        do.call(
            what = plotTotalCounts,
            args = matchArgsToDoCall(
                args = list(object = aggregateCellsToSamples(object))
            )
        )
    }

formals(plotTotalCounts.SingleCellExperiment) <-
    formals(plotTotalCounts.SummarizedExperiment)



#' @rdname plotTotalCounts
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature("SummarizedExperiment"),
    definition = plotTotalCounts.SummarizedExperiment
)



#' @rdname plotTotalCounts
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature("SingleCellExperiment"),
    definition = plotTotalCounts.SingleCellExperiment
)
