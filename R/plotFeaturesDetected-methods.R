#' @name plotFeaturesDetected
#' @inherit bioverbs::plotFeaturesDetected
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



## Updated 2019-08-21.
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
        counts <- assay(object, i = assay)
        ## Keep this calculation sparse, if necessary, for speed.
        if (is(counts, "sparseMatrix")) {
            colSums <- Matrix::colSums
        }
        featureCount <- colSums(counts >= minCounts)
        data <- metrics(object)
        data[["featureCount"]] <- featureCount
        ## Plot.
        data <- as_tibble(data, rownames = NULL)
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
        ## Show limit line.
        if (isPositive(limit)) {
            p <- p + acid_geom_abline(yintercept = limit)
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

formals(`plotFeaturesDetected,SummarizedExperiment`)[c("fill", "flip")] <-
    formalsList[c("fill.discrete", "flip")]



#' @rdname plotFeaturesDetected
#' @export
setMethod(
    f = "plotFeaturesDetected",
    signature = signature("SummarizedExperiment"),
    definition = `plotFeaturesDetected,SummarizedExperiment`
)



## Updated 2019-08-21.
`plotFeaturesDetected,SingleCellExperiment` <-  # nolint
    function(object) {
        object <- aggregateCellsToSamples(object)
        do.call(
            what = plotFeaturesDetected,
            args = matchArgsToDoCall(
                args = list(object = object)
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
