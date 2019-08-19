#' @name plotCountsPerFeature
#' @inherit bioverbs::plotCountsPerFeature
#' @note Updated 2019-07-29.
#'
#' @inheritParams basejump::meltCounts
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
#' rse <- RangedSummarizedExperiment
#' sce <- SingleCellExperiment
#'
#' ## SummarizedExperiment ====
#' plotCountsPerFeature(rse, geom = "boxplot")
#' plotCountsPerFeature(rse, geom = "density")
#'
#' ## SingleCellExperiment ====
#' plotCountsPerFeature(sce)
NULL



#' @rdname plotCountsPerFeature
#' @name plotCountsPerFeature
#' @importFrom bioverbs plotCountsPerFeature
#' @usage plotCountsPerFeature(object, ...)
#' @export
NULL



## Updated 2019-07-23.
`plotCountsPerFeature,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        minCounts = 1L,
        minCountsMethod,
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
            isInt(minCounts),
            isGreaterThanOrEqualTo(minCounts, 1L),
            isGGScale(color, scale = "discrete", aes = "colour", nullOK = TRUE),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isFlag(flip),
            isString(countsAxisLabel, nullOK = TRUE),
            isString(title, nullOK = TRUE)
        )
        minCountsMethod <- match.arg(minCountsMethod)
        geom <- match.arg(geom)
        trans <- match.arg(trans)

        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)

        data <- meltCounts(
            object = object,
            assay = assay,
            minCounts = minCounts,
            minCountsMethod = minCountsMethod,
            trans = trans
        )
        data <- as_tibble(data, rownames = NULL)

        ## Counts axis label. Automatically add transformation, if necessary.
        if (trans != "identity") {
            countsAxisLabel <- paste(trans, countsAxisLabel)
        }

        ## Construct the ggplot.
        p <- ggplot(data = data)

        if (geom == "density") {
            p <- p +
                geom_density(
                    mapping = aes(
                        x = !!sym("counts"),
                        group = !!sym("interestingGroups"),
                        color = !!sym("interestingGroups")
                    ),
                    fill = NA,
                    size = 1L
                ) +
                labs(x = countsAxisLabel)
        } else if (geom == "boxplot") {
            p <- p +
                geom_boxplot(
                    mapping = aes(
                        x = !!sym("sampleName"),
                        y = !!sym("counts"),
                        fill = !!sym("interestingGroups")
                    ),
                    color = "black"
                ) +
                labs(x = NULL, y = countsAxisLabel)
        } else if (geom == "jitter") {
            p <- p +
                geom_jitter(
                    mapping = aes(
                        x = !!sym("sampleName"),
                        y = !!sym("counts"),
                        color = !!sym("interestingGroups")
                    ),
                    size = 0.5
                ) +
                labs(x = NULL, y = countsAxisLabel)
        }

        ## Subtitle
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

        if (geom == "boxplot") {
            if (is(fill, "ScaleDiscrete")) {
                p <- p + fill
            }
        } else if (geom %in% c("density", "jitter")) {
            if (is(color, "ScaleDiscrete")) {
                p <- p + color
            }
        }

        ## Flip the axis for plots with counts on y-axis, if desired.
        if (isTRUE(flip) && !geom %in% "density") {
            p <- acid_coord_flip(p)
        }

        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(color = FALSE, fill = FALSE)
        }

        p
    }

formals(`plotCountsPerFeature,SummarizedExperiment`)[["color"]] <-
    formalsList[["color.discrete"]]
formals(`plotCountsPerFeature,SummarizedExperiment`)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(`plotCountsPerFeature,SummarizedExperiment`)[["flip"]] <-
    formalsList[["flip"]]
formals(`plotCountsPerFeature,SummarizedExperiment`)[["minCountsMethod"]] <-
    methodFormals(
        f = "meltCounts",
        signature = "SummarizedExperiment",
        package = "basejump"
    )[["minCountsMethod"]]



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
        do.call(
            what = plotCountsPerFeature,
            args = matchArgsToDoCall(
                args = list(
                    object = aggregateCellsToSamples(object)
                )
            )
        )
    }

formals(`plotCountsPerFeature,SingleCellExperiment`) <-
    formals(`plotCountsPerFeature,SummarizedExperiment`)



#' @rdname plotCountsPerFeature
#' @export
setMethod(
    f = "plotCountsPerFeature",
    signature = signature("SingleCellExperiment"),
    definition = `plotCountsPerFeature,SingleCellExperiment`
)
