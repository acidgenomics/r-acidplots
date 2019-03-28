#' @name plotGenesDetected
#' @inherit bioverbs::plotGenesDetected
#' @inheritParams params
#' @examples
#' data(rse, sce, package = "acidtest")
#' plotGenesDetected(rse)
#' plotGenesDetected(sce)
NULL



#' @importFrom bioverbs plotGenesDetected
#' @aliases NULL
#' @export
bioverbs::plotGenesDetected



plotGenesDetected.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        limit = 0L,
        minCounts = 1L,
        fill,
        flip,
        title = "genes detected"
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isInt(limit) && isNonNegative(limit),
            isInt(minCounts) && isNonNegative(minCounts),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isFlag(flip),
            isString(title, nullOK = TRUE)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)

        counts <- assays(object)[[assay]]

        # Keep this calculation sparse, if necessary, for speed.
        if (is(counts, "sparseMatrix")) {
            colSums <- Matrix::colSums
        }
        geneCount <- colSums(counts >= minCounts)

        data <- metrics(object) %>%
            as_tibble() %>%
            mutate(geneCount = !!geneCount)

        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("sampleName"),
                y = !!sym("geneCount"),
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
                y = "gene count",
                fill = paste(interestingGroups, collapse = ":\n")
            )

        if (isPositive(limit)) {
            p <- p + acid_geom_abline(yintercept = limit)
        }

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

formals(plotGenesDetected.SummarizedExperiment)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(plotGenesDetected.SummarizedExperiment)[["flip"]] <-
    formalsList[["flip"]]



#' @rdname plotGenesDetected
#' @export
setMethod(
    f = "plotGenesDetected",
    signature = signature("SummarizedExperiment"),
    definition = plotGenesDetected.SummarizedExperiment
)



plotGenesDetected.SingleCellExperiment <-  # nolint
    function(object) {
        do.call(
            what = plotGenesDetected,
            args = matchArgsToDoCall(
                args = list(object = aggregateCellsToSamples(object))
            )
        )
    }

formals(plotGenesDetected.SingleCellExperiment) <-
    formals(plotGenesDetected.SummarizedExperiment)



#' @rdname plotGenesDetected
#' @export
setMethod(
    f = "plotGenesDetected",
    signature = signature("SingleCellExperiment"),
    definition = plotGenesDetected.SingleCellExperiment
)
