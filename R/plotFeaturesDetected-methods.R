#' @name plotFeaturesDetected
#' @inherit bioverbs::plotFeaturesDetected
#' @inheritParams params
#' @examples
#' data(rse, sce, package = "acidtest")
#' plotFeaturesDetected(rse)
#' plotFeaturesDetected(sce)
NULL



#' @rdname plotFeaturesDetected
#' @name plotFeaturesDetected
#' @importFrom bioverbs plotFeaturesDetected
#' @export
NULL



plotFeaturesDetected.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        limit = 0L,
        minCounts = 1L,
        fill,
        flip,
        title = "Features detected",
        countsAxisLabel = "features"
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isInt(limit) && isNonNegative(limit),
            isInt(minCounts) && isNonNegative(minCounts),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isFlag(flip),
            isString(title, nullOK = TRUE),
            isString(countsAxisLabel)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)

        counts <- assays(object)[[assay]]

        # Keep this calculation sparse, if necessary, for speed.
        if (is(counts, "sparseMatrix")) {
            colSums <- Matrix::colSums
        }
        featureCount <- colSums(counts >= minCounts)

        data <- metrics(object) %>%
            as_tibble() %>%
            mutate(featureCount = !!featureCount)

        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("sampleName"),
                y = !!sym("featureCount"),
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
                y = countsAxisLabel,
                fill = paste(interestingGroups, collapse = ":\n")
            )

        if (isPositive(limit)) {
            p <- p + acid_geom_abline(yintercept = limit)
        }

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

formals(plotFeaturesDetected.SummarizedExperiment)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(plotFeaturesDetected.SummarizedExperiment)[["flip"]] <-
    formalsList[["flip"]]



#' @rdname plotFeaturesDetected
#' @export
setMethod(
    f = "plotFeaturesDetected",
    signature = signature("SummarizedExperiment"),
    definition = plotFeaturesDetected.SummarizedExperiment
)



plotFeaturesDetected.SingleCellExperiment <-  # nolint
    function(object) {
        do.call(
            what = plotFeaturesDetected,
            args = matchArgsToDoCall(
                args = list(object = aggregateCellsToSamples(object))
            )
        )
    }

formals(plotFeaturesDetected.SingleCellExperiment) <-
    formals(plotFeaturesDetected.SummarizedExperiment)



#' @rdname plotFeaturesDetected
#' @export
setMethod(
    f = "plotFeaturesDetected",
    signature = signature("SingleCellExperiment"),
    definition = plotFeaturesDetected.SingleCellExperiment
)



#' @rdname plotFeaturesDetected
#' @export
plotGenesDetected <- function(...) {
    plotFeaturesDetected(
        ...,
        title = "Genes detected",
        countsAxisLabel = "genes"
    )
}
