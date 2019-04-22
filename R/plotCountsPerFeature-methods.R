#' @name plotCountsPerFeature
#' @inherit bioverbs::plotCountsPerFeature
#' @inheritParams basejump::meltCounts
#' @inheritParams params
#'
#' @param geom `character(1)`.
#'   Type of ggplot2 geometric object to use.
#'
#' @examples
#' data(rse, sce, package = "acidtest")
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
#' @export
NULL



plotCountsPerFeature.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        minCounts = 1L,
        minCountsMethod,
        interestingGroups = NULL,
        geom = c("boxplot", "density", "violin"),
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

        # Counts axis label. Automatically add transformation, if necessary.
        if (trans != "identity") {
            countsAxisLabel <- paste(trans, countsAxisLabel)
        }

        # Construct the ggplot.
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
        } else if (geom == "violin") {
            p <- p +
                geom_violin(
                    mapping = aes(
                        x = !!sym("sampleName"),
                        y = !!sym("counts"),
                        fill = !!sym("interestingGroups")
                    ),
                    color = "black",
                    scale = "width"
                ) +
                labs(x = NULL, y = countsAxisLabel)
        }

        # Subtitle
        if (isString(title)) {
            count <- length(unique(data[["rowname"]]))
            subtitle <- paste(count, "features")
        } else {
            subtitle <- NULL
        }

        # Add the axis and legend labels.
        p <- p +
            labs(
                title = title,
                subtitle = subtitle,
                color = paste(interestingGroups, collapse = ":\n"),
                fill = paste(interestingGroups, collapse = ":\n")
            )

        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }

        # Flip the axis for plots with counts on y-axis, if desired.
        if (isTRUE(flip) && !geom %in% "density") {
            p <- acid_coord_flip(p)
        }

        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(color = FALSE, fill = FALSE)
        }

        p
    }

formals(plotCountsPerFeature.SummarizedExperiment)[["color"]] <-
    formalsList[["color.discrete"]]
formals(plotCountsPerFeature.SummarizedExperiment)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(plotCountsPerFeature.SummarizedExperiment)[["flip"]] <-
    formalsList[["flip"]]
formals(plotCountsPerFeature.SummarizedExperiment)[["minCountsMethod"]] <-
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
    definition = plotCountsPerFeature.SummarizedExperiment
)



plotCountsPerFeature.SingleCellExperiment <-  # nolint
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

formals(plotCountsPerFeature.SingleCellExperiment) <-
    formals(plotCountsPerFeature.SummarizedExperiment)



#' @rdname plotCountsPerFeature
#' @export
setMethod(
    f = "plotCountsPerFeature",
    signature = signature("SingleCellExperiment"),
    definition = plotCountsPerFeature.SingleCellExperiment
)
