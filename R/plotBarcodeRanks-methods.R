#' @name plotBarcodeRanks
#' @inherit AcidGenerics::plotBarcodeRanks
#' @note Requires DropletUtils package to be installed.
#' @note Updated 2019-09-15.
#'
#' @param colors `character(3)`.
#'   Character vector denoting `fitline`, `inflection`, and `knee` point colors.
#'   Must pass in color names or hexadecimal values.
#' @param ... Passthrough to [barcodeRanksPerSample()].
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' if (requireNamespace("DropletUtils", quietly = TRUE)) {
#'     object <- SingleCellExperiment
#'     plotBarcodeRanks(object)
#' }
NULL



#' @rdname plotBarcodeRanks
#' @name plotBarcodeRanks
#' @importFrom AcidGenerics plotBarcodeRanks
#' @usage plotBarcodeRanks(object, ...)
#' @export
NULL



## Updated 2019-09-15.
`plotBarcodeRanks,SingleCellExperiment` <-  # nolint
    function(
        object,
        colors = c(
            fitline = acidplots::lightPalette[["blue"]],
            inflection = acidplots::lightPalette[["purple"]],
            knee = acidplots::lightPalette[["orange"]]
        ),
        ...
    ) {
        validObject(object)
        dots <- list(...)
        assert(
            requireNamespace("DropletUtils", quietly = TRUE),
            isCharacter(colors),
            areSetEqual(
                x = names(colors),
                y = c("fitline", "inflection", "knee")
            )
        )
        ranksPerSample <- do.call(
            what = barcodeRanksPerSample,
            args = c(object = object, dots)
        )
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
                ## Include the fit line (smooth.spline).
                fitData <- data
                keep <- which(!is.na(fitData[["fitted"]]))
                fitData <- fitData[keep, , drop = FALSE]
                p <- p + geom_line(
                    data = fitData,
                    mapping = aes(
                        x = !!sym("rank"),
                        y = !!sym("fitted")
                    ),
                    color = colors[["fitline"]],
                    size = 1L
                )
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
                ## Label the knee and inflection points more clearly
                knee <- which.min(abs(data[["total"]] - knee))
                inflection <- which.min(abs(data[["total"]] - inflection))
                labelData <- data[c(knee, inflection), , drop = FALSE]
                labelData[["label"]] <- c(
                    paste("knee", "=", knee),
                    paste("inflection", "=", inflection)
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
        plot_grid(plotlist = plotlist)
    }



#' @rdname plotBarcodeRanks
#' @export
setMethod(
    f = "plotBarcodeRanks",
    signature = signature("SingleCellExperiment"),
    definition = `plotBarcodeRanks,SingleCellExperiment`
)
