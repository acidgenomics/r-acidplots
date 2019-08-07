#' @name plotFeaturesDetected
#' @inherit bioverbs::plotFeaturesDetected
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
#' plotFeaturesDetected(rse)
#'
#' ## SingleCellExperiment ====
#' plotFeaturesDetected(sce)
NULL



#' @rdname plotFeaturesDetected
#' @name plotFeaturesDetected
#' @importFrom bioverbs plotFeaturesDetected
#' @usage plotFeaturesDetected
#' @export
NULL



## Updated 2019-07-23.
`plotFeaturesDetected,SummarizedExperiment` <-  # nolint
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

        ## Keep this calculation sparse, if necessary, for speed.
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
            acid_geom_bar() +
            acid_scale_y_continuous_nopad() +
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

formals(`plotFeaturesDetected,SummarizedExperiment`)[c("fill", "flip")] <-
    formalsList[c("fill.discrete", "flip")]



#' @rdname plotFeaturesDetected
#' @export
setMethod(
    f = "plotFeaturesDetected",
    signature = signature("SummarizedExperiment"),
    definition = `plotFeaturesDetected,SummarizedExperiment`
)



## Updated 2019-07-23.
`plotFeaturesDetected,SingleCellExperiment` <-  # nolint
    function(object) {
        do.call(
            what = plotFeaturesDetected,
            args = matchArgsToDoCall(
                args = list(object = aggregateCellsToSamples(object))
            )
        )
    }

formals(`plotFeaturesDetected,SingleCellExperiment`) <-
    formals(`plotFeaturesDetected,SummarizedExperiment`)



#' @rdname plotFeaturesDetected
#' @export
setMethod(
    f = "plotFeaturesDetected",
    signature = signature("SingleCellExperiment"),
    definition = `plotFeaturesDetected,SingleCellExperiment`
)
