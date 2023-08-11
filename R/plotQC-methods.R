#' @name plotQC
#' @inherit AcidGenerics::plotQC
#' @note Updated 2023-08-11.
#'
#' @inheritParams AcidRoxygen::params
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
#' plotQC(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' plotQC(object)
NULL



## Updated 2023-08-11.
`plotQC,SE` <- # nolint
    function(object,
             assay = 1L,
             interestingGroups = NULL,
             legend,
             labels = list(
                 "title" = "Quality control",
                 "subtitle" = NULL
             )) {
        validObject(object)
        assert(
            isScalar(assay),
            isFlag(legend)
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        labels <- matchLabels(labels)
        ## Construct the plotlist.
        plotlist <- list()
        plotlist[["totalCounts"]] <-
            plotTotalCounts(
                object = object,
                assay = assay
            )
        plotlist[["zerosVsDepth"]] <-
            plotZerosVsDepth(
                object = object,
                assay = assay
            ) +
            guides(color = "none")
        plotlist[["rowSums"]] <-
            plotSums(
                object = object,
                assay = assay,
                MARGIN = 1L
            ) +
            theme(legend.position = "none")
        plotlist[["colSums"]] <-
            plotSums(
                object = object,
                assay = assay,
                MARGIN = 2L
            ) +
            theme(legend.position = "none")
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

formals(`plotQC,SE`)[["legend"]] <- # nolint
    .formalsList[["legend"]]



## Updated 2023-08-11.
`plotQC,SCE` <- # nolint
    function(object,
             assay = 1L,
             interestingGroups = NULL,
             geom,
             legend,
             labels = list(
                 "title" = "Quality control",
                 "subtitle" = NULL
             )) {
        validObject(object)
        assert(
            identical(assayNames(object)[[1L]], "counts"),
            isScalar(assay),
            isFlag(legend)
        )
        if (!hasMetrics(object)) {
            suppressMessages({
                object <- calculateMetrics(object, assay = assay)
            })
        }
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        geom <- match.arg(geom)
        plotlist <- list()
        ## Don't show cell counts for unfiltered datasets.
        if (hasSubset(object, metadata = "filterCells")) {
            plotlist[["cellCounts"]] <-
                plotCellCounts(object) +
                theme(legend.position = "none")
        } else {
            plotlist[["zerosVsDepth"]] <-
                plotZerosVsDepth(object, assay = assay) +
                theme(legend.position = "none")
        }
        plotlist[["countsPerCell"]] <-
            plotCountsPerCell(
                object = object,
                geom = geom
            )
        plotlist[["featuresPerCell"]] <-
            plotFeaturesPerCell(
                object = object,
                geom = geom
            ) +
            theme(legend.position = "none")
        plotlist[["countsVsFeatures"]] <-
            plotCountsVsFeatures(object) +
            theme(legend.position = "none")
        plotlist[["novelty"]] <-
            plotNovelty(
                object = object,
                geom = geom
            ) +
            theme(legend.position = "none")
        plotlist[["mitoRatio"]] <-
            tryCatch(
                expr = {
                    plotMitoRatio(
                        object = object,
                        geom = geom
                    ) +
                        theme(legend.position = "none")
                },
                error = function(e) {
                    NULL
                }
            )
        plotlist[["rowSums"]] <-
            plotSums(
                object = object,
                assay = assay,
                MARGIN = 1L
            ) +
            theme(legend.position = "none")
        plotlist[["colSums"]] <-
            plotSums(
                object = object,
                assay = assay,
                MARGIN = 2L
            ) +
            theme(legend.position = "none")
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

formals(`plotQC,SCE`)[c("geom", "legend")] <- # nolint
    .formalsList[c("geom", "legend")]



#' @rdname plotQC
#' @export
setMethod(
    f = "plotQC",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotQC,SCE`
)

#' @rdname plotQC
#' @export
setMethod(
    f = "plotQC",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotQC,SE`
)
