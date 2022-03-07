#' @name plotBarcodeRanks
#' @inherit AcidGenerics::plotBarcodeRanks
#' @note Requires DropletUtils package to be installed.
#' @note Updated 2022-01-19.
#'
#' @inheritParams AcidRoxygen::params
#' @param colors `character(3)`.
#'   Character vector denoting `fitline`, `inflection`, and `knee` point colors.
#'   Must pass in color names or hexadecimal values.
#' @param ... Passthrough to [barcodeRanksPerSample()].
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' if (requireNamespace("DropletUtils", quietly = TRUE)) {
#'     object <- SingleCellExperiment_splatter
#'     plotBarcodeRanks(object)
#' }
NULL



## Updated 2021-08-11.
`plotBarcodeRanks,SCE` <-  # nolint
    function(
        object,
        colors = c(
            "fitline" = AcidPlots::lightPalette[["blue"]],
            "inflection" = AcidPlots::lightPalette[["purple"]],
            "knee" = AcidPlots::lightPalette[["orange"]]
        ),
        labels = list(
            "title" = NULL,
            "subtitle" = NULL
        ),
        ...
    ) {
        requireNamespaces("DropletUtils")
        validObject(object)
        assert(
            isCharacter(colors),
            areSetEqual(
                x = names(colors),
                y = c("fitline", "inflection", "knee")
            )
        )
        labels <- matchLabels(labels)
        ranksPerSample <- barcodeRanksPerSample(object, ...)
        sampleData <- sampleData(object)
        if (is.null(sampleData)) {
            sampleNames <- "unknown"
        } else {
            sampleNames <- sampleData(object)[
                names(ranksPerSample),
                "sampleName",
                drop = TRUE
                ]
            sampleNames <- as.character(sampleNames)
        }
        plotlist <- mapply(
            sampleName = sampleNames,
            ranks = ranksPerSample,
            FUN = function(sampleName, ranks) {
                data <- as_tibble(ranks, rownames = NULL)
                inflection <- metadata(ranks)[["inflection"]]
                knee <- metadata(ranks)[["knee"]]
                ## Label the knee and inflection points more clearly
                whichKnee <- which.min(abs(data[["total"]] - knee))
                whichInflection <- which.min(abs(data[["total"]] - inflection))
                labelData <- data[c(whichKnee, whichInflection), , drop = FALSE]
                labelData[["label"]] <- c(
                    paste("knee", "=", knee),
                    paste("inflection", "=", inflection)
                )
                ## Include the fit line (smooth.spline), if possible.
                fitData <- data
                keep <- which(!is.na(fitData[["fitted"]]))
                fitData <- fitData[keep, , drop = FALSE]
                p <- ggplot(data = data) +
                    geom_point(
                        mapping = aes(
                            x = !!sym("rank"),
                            y = !!sym("total")
                        )
                    ) +
                    scale_x_continuous(trans = "log10") +
                    scale_y_continuous(trans = "log10") +
                    labs(
                        title = sampleName,
                        y = "counts per cell"
                    )
                if (hasRows(fitData)) {
                    p <- p + geom_line(
                        data = fitData,
                        mapping = aes(
                            x = !!sym("rank"),
                            y = !!sym("fitted")
                        ),
                        color = colors[["fitline"]],
                        size = 1L
                    )
                }
                p <- p +
                    geom_hline(
                        color = colors[["knee"]],
                        linetype = "dashed",
                        yintercept = knee
                    ) +
                    geom_hline(
                        color = colors[["inflection"]],
                        linetype = "dashed",
                        yintercept = inflection
                    )
                p +
                    acid_geom_label_repel(
                        data = labelData,
                        mapping = aes(
                            x = !!sym("rank"),
                            y = !!sym("total"),
                            label = !!sym("label")
                        ),
                        color = colors[c("knee", "inflection")]
                    )
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        )
        ## Sort the plots by sample name.
        plotlist <- plotlist[sort(names(plotlist))]
        ## Using patchwork package to dynamically arrange the plots.
        p <- wrap_plots(plotlist)
        ## Support title and/or subtitle labeling.
        p <- p + plot_annotation(
            "title" = labels[["title"]],
            "subtitle" = labels[["subtitle"]]
        )
        p
    }



#' @rdname plotBarcodeRanks
#' @export
setMethod(
    f = "plotBarcodeRanks",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotBarcodeRanks,SCE`
)
