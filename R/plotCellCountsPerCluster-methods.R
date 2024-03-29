#' @name plotCellCountsPerCluster
#' @inherit AcidGenerics::plotCellCountsPerCluster
#' @note Updated 2023-08-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Show graphical output. Invisibly return `ggplot`.
#'
#' @examples
#' data(SingleCellExperiment_Seurat, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_Seurat
#' plotCellCountsPerCluster(object)
NULL



## Updated 2023-08-10.
`plotCellCountsPerCluster,SCE` <- # nolint
    function(object,
             interestingGroups = NULL) {
        validObject(object)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        data <- cellCountsPerCluster(object = object)
        if (isTRUE(length(levels(data[["sampleName"]])) > 1L)) {
            multipleSamples <- TRUE
            col <- "interestingGroups"
            legendTitle <- paste(interestingGroups, collapse = ":\n")
            showLegend <- TRUE
            xLab <- NULL
        } else {
            multipleSamples <- FALSE
            col <- "ident"
            legendTitle <- NA
            showLegend <- FALSE
            xLab <- "cluster"
        }
        ## Plot.
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[[col]],
                y = .data[["n"]],
                fill = .data[[col]]
            )
        ) +
            geom_bar(
                stat = "identity",
                show.legend = showLegend
            ) +
            labs(
                x = xLab,
                y = "n cells",
                fill = legendTitle
            )
        ## Color palette.
        p <- p + acid_scale_fill_discrete()
        ## Wrap for multiple samples.
        if (isTRUE(multipleSamples)) {
            p <- p + facet_wrap(facets = vars(.data[["ident"]]))
        }
        ## Return.
        p
    }



#' @rdname plotCellCountsPerCluster
#' @export
setMethod(
    f = "plotCellCountsPerCluster",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotCellCountsPerCluster,SCE`
)
