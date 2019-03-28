#' @name plotCountsPerGene
#' @inherit bioverbs::plotCountsPerGene
#' @inheritParams params
#'
#' @param geom `character(1)`.
#'   Type of ggplot2 geometric object to use.
#'
#' @examples
#' data(rse, sce, package = "acidtest")
#'
#' ## SummarizedExperiment ====
#' plotCountsPerGene(rse, geom = "boxplot")
#' plotCountsPerGene(rse, geom = "density")
#'
#' ## SingleCellExperiment ====
#' plotCountsPerGene(sce)
NULL



#' @importFrom bioverbs plotCountsPerGene
#' @aliases NULL
#' @export
bioverbs::plotCountsPerGene



plotCountsPerGene.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        geom = c("boxplot", "density", "violin"),
        trans = c("identity", "log2", "log10"),
        color,
        fill,
        flip,
        countsAxisLabel = "counts",
        title = "counts per gene"
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isFlag(flip),
            isString(countsAxisLabel, nullOK = TRUE),
            isString(title, nullOK = TRUE)
        )
        geom <- match.arg(geom)
        trans <- match.arg(trans)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)

        data <- meltCounts(
            object = object,
            assay = assay,
            nonzeroGenes = TRUE,
            trans = trans
        )

        # Subtitle
        if (isString(title)) {
            count <- length(unique(data[["rowname"]]))
            subtitle <- paste(count, "non-zero genes")
        } else {
            subtitle <- NULL
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
            p <- p + coord_flip()
        }

        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(color = FALSE, fill = FALSE)
        }

        p
    }

formals(plotCountsPerGene.SummarizedExperiment)[["color"]] <-
    formalsList[["color.discrete"]]
formals(plotCountsPerGene.SummarizedExperiment)[["fill"]] <-
    formalsList[["fill.discrete"]]
formals(plotCountsPerGene.SummarizedExperiment)[["flip"]] <-
    formalsList[["flip"]]


plotCountsPerGene.SingleCellExperiment <-  # nolint
    function(object) {
        do.call(
            what = plotCountsPerGene,
            args = matchArgsToDoCall(
                args = list(
                    object = aggregateCellsToSamples(object)
                )
            )
        )
    }
formals(plotCountsPerGene.SingleCellExperiment) <-
    formals(plotCountsPerGene.SummarizedExperiment)



#' @rdname plotCountsPerGene
#' @export
setMethod(
    f = "plotCountsPerGene",
    signature = signature("SummarizedExperiment"),
    definition = plotCountsPerGene.SummarizedExperiment
)



#' @rdname plotCountsPerGene
#' @export
setMethod(
    f = "plotCountsPerGene",
    signature = signature("SingleCellExperiment"),
    definition = plotCountsPerGene.SingleCellExperiment
)
