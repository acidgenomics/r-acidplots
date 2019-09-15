#' @name plotCountsPerFeature
#' @inherit bioverbs::plotCountsPerFeature
#' @note Updated 2019-08-27.
#'
#' @inheritParams basejump::melt
#' @inheritParams acidroxygen::params
#' @param geom `character(1)`.
#'   Type of ggplot2 geometric object to use.
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
#' plotCountsPerFeature(object, geom = "boxplot")
#' plotCountsPerFeature(object, geom = "density")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotCountsPerFeature(object)
NULL



#' @rdname plotCountsPerFeature
#' @name plotCountsPerFeature
#' @importFrom bioverbs plotCountsPerFeature
#' @usage plotCountsPerFeature(object, ...)
#' @export
NULL



## Updated 2019-08-27.
`plotCountsPerFeature,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        min = 1L,
        minMethod,
        interestingGroups = NULL,
        geom = c("boxplot", "density", "jitter"),
        trans = c("identity", "log2", "log10"),
        color,
        fill,
        flip,
        countsAxisLabel = "counts",
        title = "Counts per feature"
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isInt(min),
            isGreaterThanOrEqualTo(min, 1L),
            isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isFlag(flip),
            isString(countsAxisLabel, nullOK = TRUE),
            isString(title, nullOK = TRUE)
        )
        minMethod <- match.arg(minMethod)
        geom <- match.arg(geom)
        trans <- match.arg(trans)
        if (!identical(trans, "identity")) {
            countsAxisLabel <- paste(trans, countsAxisLabel)
        }
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        data <- melt(
            object = object,
            assay = assay,
            min = min,
            minMethod = minMethod,
            trans = trans
        )
        ## Construct the ggplot.
        data <- as_tibble(data, rownames = NULL)
        p <- ggplot(data = data)
        if (identical(geom, "density")) {
            p <- p +
                geom_density(
                    mapping = aes(
                        x = !!sym("value"),
                        group = !!sym("interestingGroups"),
                        color = !!sym("interestingGroups")
                    ),
                    fill = NA,
                    size = 1L
                ) +
                labs(x = countsAxisLabel)
        } else if (identical(geom, "boxplot")) {
            p <- p +
                geom_boxplot(
                    mapping = aes(
                        x = !!sym("sampleName"),
                        y = !!sym("value"),
                        fill = !!sym("interestingGroups")
                    ),
                    color = "black"
                ) +
                labs(x = NULL, y = countsAxisLabel)
        } else if (identical(geom, "jitter")) {
            p <- p +
                geom_jitter(
                    mapping = aes(
                        x = !!sym("sampleName"),
                        y = !!sym("value"),
                        color = !!sym("interestingGroups")
                    ),
                    size = 0.5
                ) +
                labs(x = NULL, y = countsAxisLabel)
        }
        ## Subtitle.
        if (isString(title)) {
            count <- length(unique(data[["rowname"]]))
            subtitle <- paste("n", "=", count)
        } else {
            subtitle <- NULL
        }
        ## Add the axis and legend labels.
        p <- p +
            labs(
                title = title,
                subtitle = subtitle,
                color = paste(interestingGroups, collapse = ":\n"),
                fill = paste(interestingGroups, collapse = ":\n")
            )
        if (identical(geom, "boxplot")) {
            if (is(fill, "ScaleDiscrete")) {
                p <- p + fill
            }
        } else if (isSubset(geom, c("density", "jitter"))) {
            if (is(color, "ScaleDiscrete")) {
                p <- p + color
            }
        }
        ## Flip the axis for plots with counts on y-axis, if desired.
        if (isTRUE(flip) && !identical(geom, "density")) {
            p <- acid_coord_flip(p)
        }
        ## Hide sample name legend.
        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(color = FALSE, fill = FALSE)
        }
        ## Return.
        p
    }

formals(`plotCountsPerFeature,SummarizedExperiment`)[["color"]] <-
    formalsList[["color.discrete"]]
formals(`plotCountsPerFeature,SummarizedExperiment`)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(`plotCountsPerFeature,SummarizedExperiment`)[["flip"]] <-
    formalsList[["flip"]]
formals(`plotCountsPerFeature,SummarizedExperiment`)[["minMethod"]] <-
    methodFormals(
        f = "melt",
        signature = "SummarizedExperiment",
        package = "basejump"
    )[["minMethod"]]



#' @rdname plotCountsPerFeature
#' @export
setMethod(
    f = "plotCountsPerFeature",
    signature = signature("SummarizedExperiment"),
    definition = `plotCountsPerFeature,SummarizedExperiment`
)



## Updated 2019-07-23.
`plotCountsPerFeature,SingleCellExperiment` <-  # nolint
    function(object) {
        object <- aggregateCellsToSamples(object)
        do.call(
            what = plotCountsPerFeature,
            args = matchArgsToDoCall(
                args = list(object = object)
            )
        )
    }

formals(`plotCountsPerFeature,SingleCellExperiment`) <-
    formals(`plotCountsPerFeature,SummarizedExperiment`)



#' @describeIn plotCountsPerFeature Applies [aggregateCellsToSamples()]
#'   calculation to summarize at sample level prior to plotting.
#' @export
setMethod(
    f = "plotCountsPerFeature",
    signature = signature("SingleCellExperiment"),
    definition = `plotCountsPerFeature,SingleCellExperiment`
)
