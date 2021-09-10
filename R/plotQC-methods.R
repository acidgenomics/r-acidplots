## FIXME This needs to support `interestingGroups` argument.



#' @name plotQC
#' @inherit AcidGenerics::plotQC
#' @note Updated 2021-09-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotQC(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- calculateMetrics(object)
#' plotQC(object)
NULL



## Updated 2021-08-11.
`plotQC,SE` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        legend,
        labels = list(
            "title" = "Quality control",
            "subtitle" = NULL
        )
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isFlag(legend)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        labels <- matchLabels(labels)
        plotlist <- list(
            "totalCounts" =
                plotTotalCounts(object, assay = assay),
            "zerosVsDepth" =
                plotZerosVsDepth(object, assay = assay) +
                guides(color = "none"),
            "rowSums" =
                ## FIXME Need to batch these per interesting group.
                ## FIXME Need to make the labels more descriptive.
                plotSums(object, assay = assay, MARGIN = 1L),
            "colSums" =
                ## FIXME Need to batch these per interesting group.
                ## FIXME Need to make the labels more descriptive.
                plotSums(object, assay = assay, MARGIN = 2L)
        )
        plotlist <- Filter(f = Negate(is.null), x = plotlist)
        ## Hide the legends, if desired.
        if (identical(legend, FALSE)) {
            plotlist <- .hideLegendsInPlotlist(plotlist)
        }
        ## Using patchwork package to dynamically arrange the plots.
        p <- wrap_plots(plotlist, guides = "collect")
        ## Support title and/or subtitle labeling.
        p <- p + plot_annotation(
            "title" = labels[["title"]],
            "subtitle" = labels[["subtitle"]]
        )
        p
    }

formals(`plotQC,SE`)[["legend"]] <- formalsList[["legend"]]



## Updated 2021-08-11.
`plotQC,SCE` <-  # nolint
    function(
        object,
        geom,
        legend,
        labels = list(
            "title" = "Quality control",
            "subtitle" = NULL
        )
    ) {
        validObject(object)
        assert(
            hasMetrics(object),
            identical(assayNames(object)[[1L]], "counts"),
            isFlag(legend)
        )
        geom <- match.arg(geom)
        plotlist <- list()
        ## Don't show cell counts for unfiltered datasets.
        if (hasSubset(object, metadata = "filterCells")) {
            plotlist[["cellCounts"]] <-
                plotCellCounts(object) +
                theme(legend.position = "none")
        } else {
            plotlist[["zerosVsDepth"]] <-
                plotZerosVsDepth(object) +
                theme(legend.position = "none")
        }
        plotlist <- append(
            x = plotlist,
            values = list(
                "countsPerCell" =
                    plotCountsPerCell(object, geom = geom),
                "featuresPerCell" =
                    plotFeaturesPerCell(object, geom = geom) +
                    theme(legend.position = "none"),
                "countsVsFeatures" =
                    plotCountsVsFeatures(object) +
                    theme(legend.position = "none"),
                "novelty" =
                    plotNovelty(object, geom = geom) +
                    theme(legend.position = "none"),
                "mitoRatio" = tryCatch(
                    expr = {
                        plotMitoRatio(object, geom = geom) +
                        theme(legend.position = "none")
                    },
                    error = function(e) NULL
                ),
                "rowSums" =
                    ## FIXME Need to batch these per interesting group.
                    ## FIXME Need to make the labels more descriptive.
                    plotSums(
                        object = object,
                        assay = 1L,
                        MARGIN = 1L
                    ),
                "colSums" =
                    ## FIXME Need to batch these per interesting group.
                    ## FIXME Need to make the labels more descriptive.
                    plotSums(
                        object = object,
                        assay = 1L,
                        MARGIN = 2L
                    )
            )
        )
        plotlist <- Filter(f = Negate(is.null), x = plotlist)
        ## Hide the legends, if desired.
        if (identical(legend, FALSE)) {
            plotlist <- .hideLegendsInPlotlist(plotlist)
        }
        ## Using patchwork package to dynamically arrange the plots.
        p <- wrap_plots(plotlist, guides = "collect")
        ## Support title and/or subtitle labeling.
        p <- p + plot_annotation(
            "title" = labels[["title"]],
            "subtitle" = labels[["subtitle"]]
        )
        p
    }

formals(`plotQC,SCE`)[c("geom", "legend")] <-
    list(
        "geom" = .formalsList[["geom"]],
        "legend" = formalsList[["legend"]]
    )



#' @rdname plotQC
#' @export
setMethod(
    f = "plotQC",
    signature = signature("SingleCellExperiment"),
    definition = `plotQC,SCE`
)

#' @rdname plotQC
#' @export
setMethod(
    f = "plotQC",
    signature = signature("SummarizedExperiment"),
    definition = `plotQC,SE`
)
