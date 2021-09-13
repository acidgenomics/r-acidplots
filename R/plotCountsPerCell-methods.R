#' @name plotCountsPerCell
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotCountsPerCell
#' @note Updated 2021-09-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param point `character(1)`.
#'   Label either the knee or inflection points per sample.
#'   Requires `geom = "ecdf"`.
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- calculateMetrics(object)
#' plotCountsPerCell(object, geom = "violin")
#' plotCountsPerCell(object, geom = "ridgeline")
#' plotCountsPerCell(object, geom = "ecdf")
#' plotCountsPerCell(object, geom = "histogram")
#' plotCountsPerCell(object, geom = "boxplot")
NULL



## FIXME This needs to support assay.
## Updated 2021-09-10.
`plotCountsPerCell,SCE` <-  # nolint
    function(
        object,
        geom,
        interestingGroups = NULL,
        min = 0L,
        max = Inf,
        point = c("none", "inflection", "knee"),
        trans = "log10",
        title = "Counts per cell"
    ) {
        assert(isString(title, nullOK = TRUE))
        geom <- match.arg(geom)
        point <- match.arg(point)
        ## Override `interestingGroups` argument when labeling points.
        if (!identical(point, "none")) {
            interestingGroups <- "sampleName"
        }
        ## Plot.
        p <- do.call(
            what = .plotQCMetric,
            args = list(
                object = object,
                metricCol = "nCount",
                geom = geom,
                interestingGroups = interestingGroups,
                min = min,
                max = max,
                trans = trans
            )
        )
        ## Calculate barcode ranks and label inflection or knee points.
        if (!identical(point, "none")) {
            ## Require ecdf geom for now.
            assert(identical(geom, "ecdf"))
            if (length(title)) {
                p <- p + labs(subtitle = paste(point, "point per sample"))
            }
            sampleNames <- sampleNames(object)
            ranks <- barcodeRanksPerSample(object)
            ## Inflection or knee points per sample.
            points <- lapply(
                X = ranks,
                FUN = function(x) {
                    assert(is(x, "DataFrame"))
                    out <- metadata(x)[[point]]
                    assert(is.numeric(out))
                    out
                }
            )
            points <- unlist(points)
            names(points) <- names(ranks)
            assert(identical(names(sampleNames), names(points)))
            if (identical(geom, "ecdf")) {
                ## Calculate the y-intercept per sample.
                freq <- mapply(
                    sampleId = names(points),
                    point = points,
                    MoreArgs = list(
                        idCol = matchSampleColumn(object),
                        metrics = metrics(object)
                    ),
                    FUN = function(metrics, sampleId, idCol, point) {
                        nCount <- metrics[
                            metrics[[idCol]] == sampleId,
                            "nCount",
                            drop = TRUE
                        ]
                        e <- ecdf(sort(nCount))
                        e(point)
                    },
                    SIMPLIFY = TRUE,
                    USE.NAMES = TRUE
                )
                pointData <- data.frame(
                    "x" = points,
                    "y" = freq,
                    "label" = paste0(sampleNames, " (", points, ")"),
                    "sampleName" = sampleNames
                )
                p <- p +
                    geom_point(
                        data = pointData,
                        mapping = aes(
                            x = !!sym("x"),
                            y = !!sym("y"),
                            color = !!sym("sampleName")
                        ),
                        size = 5L,
                        show.legend = FALSE
                    ) +
                    acid_geom_label_repel(
                        data = pointData,
                        mapping = aes(
                            x = !!sym("x"),
                            y = !!sym("y"),
                            label = !!sym("label"),
                            color = !!sym("sampleName")
                        )
                    )
            }
        }
        ## Return.
        p <- p + labs(title = title)
        p
    }

formals(`plotCountsPerCell,SCE`)[["geom"]] <- .formalsList[["geom"]]



#' @rdname plotCountsPerCell
#' @export
setMethod(
    f = "plotCountsPerCell",
    signature = signature("SingleCellExperiment"),
    definition = `plotCountsPerCell,SCE`
)
