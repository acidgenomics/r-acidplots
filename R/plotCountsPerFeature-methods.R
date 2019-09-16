#' @name plotCountsPerFeature
#' @inherit bioverbs::plotCountsPerFeature
#' @note Updated 2019-09-16.
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



## Updated 2019-09-16.
`plotCountsPerFeature,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        geom = c("boxplot", "density", "jitter"),
        trans = c("identity", "log2", "log10"),
        color,
        fill,
        labels = list(
            title = "Counts per feature",
            subtitle = NULL,
            sampleAxis = NULL,
            countAxis = "counts"
        ),
        flip
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isFlag(flip)
        )
        minMethod <- match.arg(minMethod)
        geom <- match.arg(geom)
        trans <- match.arg(trans)
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        object <- nonzeroRowsAndCols(object)
        data <- melt(object = object, assay = assay, trans = trans)
        assert(identical(length(unique(data[["rowname"]])), nrow(object)))
        ## Add automatic subtitle, including feature count.
        if (
            isString(labels[["title"]]) &&
            is.null(labels[["subtitle"]])
        ) {
            labels[["subtitle"]] <- paste("n", "=", nrow(object), "(non-zero)")
        }
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
                )
        } else if (identical(geom, "boxplot")) {
            p <- p +
                geom_boxplot(
                    mapping = aes(
                        x = !!sym("sampleName"),
                        y = !!sym("value"),
                        fill = !!sym("interestingGroups")
                    ),
                    color = "black"
                )
        } else if (identical(geom, "jitter")) {
            p <- p +
                geom_jitter(
                    mapping = aes(
                        x = !!sym("sampleName"),
                        y = !!sym("value"),
                        color = !!sym("interestingGroups")
                    ),
                    size = 0.5
                )
        }
        ## Labels.
        if (is.list(labels)) {
            if (!identical(trans, "identity")) {
                labels[["countAxis"]] <- paste(trans, labels[["countAxis"]])
            }
            if (identical(geom, "density")) {
                names(labels)[names(labels) == "countAxis"] <- "x"
                names(labels)[names(labels) == "sampleAxis"] <- "y"
            } else {
                names(labels)[names(labels) == "countAxis"] <- "y"
                names(labels)[names(labels) == "sampleAxis"] <- "x"
            }
            labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
            labels[["fill"]] <- labels[["color"]]
            p <- p + do.call(what = labs, args = labels)
        }
        ## Fill or color.
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

f <- formals(`plotCountsPerFeature,SummarizedExperiment`)
f[["color"]] <- formalsList[["color.discrete"]]
f[["fill"]] <- formalsList[["fill.discrete"]]
f[["flip"]] <- formalsList[["flip"]]
f[["minMethod"]] <-
    methodFormals(
        f = "melt",
        signature = "SummarizedExperiment",
        package = "basejump"
    )[["minMethod"]]
formals(`plotCountsPerFeature,SummarizedExperiment`) <- f



#' @rdname plotCountsPerFeature
#' @export
setMethod(
    f = "plotCountsPerFeature",
    signature = signature("SummarizedExperiment"),
    definition = `plotCountsPerFeature,SummarizedExperiment`
)



## Updated 2019-07-23.
`plotCountsPerFeature,SingleCellExperiment` <-  # nolint
    function(object, ...) {
        plotCountsPerFeature(
            object = aggregateCellsToSamples(object),
            ...
        )
    }



#' @describeIn plotCountsPerFeature Applies [aggregateCellsToSamples()]
#'   calculation to summarize at sample level prior to plotting.\cr
#'   Passes `...` to `SummarizedExperiment` method.
#' @export
setMethod(
    f = "plotCountsPerFeature",
    signature = signature("SingleCellExperiment"),
    definition = `plotCountsPerFeature,SingleCellExperiment`
)
