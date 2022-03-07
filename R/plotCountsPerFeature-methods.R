#' @name plotCountsPerFeature
#' @inherit AcidGenerics::plotCountsPerFeature
#' @note Updated 2021-09-10.
#'
#' @inheritParams AcidExperiment::melt
#' @inheritParams AcidRoxygen::params
#' @param geom `character(1)`.
#'   Type of ggplot2 geometric object to use.
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment_splatter,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotCountsPerFeature(object, geom = "boxplot")
#' plotCountsPerFeature(object, geom = "density")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' plotCountsPerFeature(object)
NULL



## Updated 2021-09-10.
`plotCountsPerFeature,SE` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        geom = c("boxplot", "density", "jitter"),
        trans = c("identity", "log2", "log10"),
        labels = list(
            "title" = "Counts per feature",
            "subtitle" = NULL,
            "sampleAxis" = NULL,
            "countAxis" = "counts"
        ),
        flip
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isFlag(flip)
        )
        minMethod <- match.arg(minMethod)
        geom <- match.arg(geom)
        trans <- match.arg(trans)
        labels <- matchLabels(labels)
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
                        group = str_replace_na(!!sym("interestingGroups")),
                        color = str_replace_na(!!sym("interestingGroups"))
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
                        fill = str_replace_na(!!sym("interestingGroups"))
                    ),
                    color = "black"
                )
        } else if (identical(geom, "jitter")) {
            p <- p +
                geom_jitter(
                    mapping = aes(
                        x = !!sym("sampleName"),
                        y = !!sym("value"),
                        color = str_replace_na(!!sym("interestingGroups"))
                    ),
                    size = 0.5
                )
        }
        ## Labels.
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
        ## Color palette.
        p <- p + autoDiscreteColorScale()
        p <- p + autoDiscreteFillScale()
        ## Flip the axis for plots with counts on y-axis, if desired.
        if (isTRUE(flip) && !identical(geom, "density")) {
            p <- acid_coord_flip(p)
        }
        ## Hide sample name legend.
        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(color = "none", fill = "none")
        }
        ## Return.
        p
    }

formals(`plotCountsPerFeature,SE`)[
    c("flip", "minMethod")] <-
    list(
        "flip" = .formalsList[["flip"]],
        "minMethod" = methodFormals(
            f = "melt",
            signature = "SummarizedExperiment",
            package = "AcidExperiment"
        )[["minMethod"]]
    )



## Updated 2019-07-23.
`plotCountsPerFeature,SCE` <-  # nolint
    function(object, ...) {
        plotCountsPerFeature(
            object = aggregateCellsToSamples(object),
            ...
        )
    }



#' @describeIn plotCountsPerFeature Applies `aggregateCellsToSamples()`
#'   calculation to summarize at sample level prior to plotting.\cr
#'   Passes `...` to `SummarizedExperiment` method.
#' @export
setMethod(
    f = "plotCountsPerFeature",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotCountsPerFeature,SCE`
)

#' @rdname plotCountsPerFeature
#' @export
setMethod(
    f = "plotCountsPerFeature",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotCountsPerFeature,SE`
)
