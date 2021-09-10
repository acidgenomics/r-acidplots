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
                plotTotalCounts(
                    object = object,
                    assay = assay),
            "zerosVsDepth" =
                plotZerosVsDepth(
                    object = object,
                    assay = assay
                ) +
                guides(color = "none"),
            "rowSums" =
                plotSums(
                    object = object,
                    assay = assay,
                    MARGIN = 1L
                ) +
                theme(legend.position = "none"),
            "colSums" =
                plotSums(
                    object = object,
                    assay = assay,
                    MARGIN = 2L
                ) +
                theme(legend.position = "none")
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



## FIXME All of these functions need to support assay.
## Updated 2021-09-10.
`plotQC,SCE` <-  # nolint
    function(
        object,
        assay = 1L,
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
            isScalar(assay),
            isFlag(legend)
        )
        geom <- match.arg(geom)
        plotlist <- list()
        ## Don't show cell counts for unfiltered datasets.
        if (hasSubset(object, metadata = "filterCells")) {
            plotlist[["cellCounts"]] <-
                ## FIXME Needs to support assay.
                plotCellCounts(
                    object = object,
                    assay = assay
                ) +
                theme(legend.position = "none")
        } else {
            plotlist[["zerosVsDepth"]] <-
                ## FIXME Needs to support assay.
                plotZerosVsDepth(object, assay = assay) +
                theme(legend.position = "none")
        }
        plotlist <- append(
            x = plotlist,
            values = list(
                "countsPerCell" =
                    ## FIXME Needs to support assay.
                    plotCountsPerCell(
                        object = object,
                        assay = assay,
                        geom = geom
                    ),
                "featuresPerCell" =
                    ## FIXME Needs to support assay.
                    plotFeaturesPerCell(
                        object = object,
                        geom = geom
                    ) +
                    theme(legend.position = "none"),
                "countsVsFeatures" =
                    ## FIXME Needs to support assay.
                    plotCountsVsFeatures(
                        object = object,
                        assay = assay
                    ) +
                    theme(legend.position = "none"),
                "novelty" =
                    ## FIXME Needs to support assay.
                    plotNovelty(
                        object = object,
                        assay = assay,
                        geom = geom
                    ) +
                    theme(legend.position = "none"),
                "mitoRatio" = tryCatch(
                    expr = {
                        ## FIXME Needs to support assay.
                        plotMitoRatio(
                            object = object,
                            assay = assay,
                            geom = geom
                        ) +
                        theme(legend.position = "none")
                    },
                    error = function(e) NULL
                ),
                "rowSums" =
                    plotSums(
                        object = object,
                        assay = assay,
                        MARGIN = 1L
                    ) +
                    theme(legend.position = "none"),
                "colSums" =
                    plotSums(
                        object = object,
                        assay = assay,
                        MARGIN = 2L
                    ) +
                    theme(legend.position = "none")
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
