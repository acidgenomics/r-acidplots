#' @name plotCountsPerCell
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotCountsPerCell
#' @note Updated 2023-08-11.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param point `character(1)`.
#' Label either the knee or inflection points per sample.
#' Requires `geom = "ecdf"`.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' plotCountsPerCell(object, geom = "violin")
#' plotCountsPerCell(object, geom = "ridgeline")
#' plotCountsPerCell(object, geom = "ecdf")
#' plotCountsPerCell(object, geom = "histogram")
#' plotCountsPerCell(object, geom = "boxplot")
NULL



## Updated 2023-08-11.
`plotCountsPerCell,SCE` <- # nolint
    function(object,
             geom,
             interestingGroups = NULL,
             min = 0L,
             max = Inf,
             point = c("none", "inflection", "knee"),
             trans = "log10",
             title = "Counts per cell") {
        assert(isString(title, nullOK = TRUE))
        geom <- match.arg(geom)
        point <- match.arg(point)
        interestingGroups <- matchInterestingGroups(object, interestingGroups)
        if (!identical(point, "none")) {
            ## Require ecdf geom for now.
            assert(
                identical(geom, "ecdf"),
                msg = sprintf(
                    paste(
                        "Only {.var %s} {.val %s} is currently",
                        "supported for {.var %s} {.val %s}."
                    ),
                    "geom", "ecdf",
                    "point", point
                )
            )
            ## Override `interestingGroups` argument when labeling points.
            assert(
                identical(interestingGroups, "sampleName"),
                msg = sprintf(
                    paste(
                        "Only {.var %s} {.val %s} is currently supported when",
                        "labeling points with {.var %s} {.val %s}."
                    ),
                    "interestingGroups", "sampleName",
                    "point", point
                )
            )
            ## Need to ensure that `nCount` exists for downstream calculations.
            if (!isSubset("nCount", colnames(colData(object)))) {
                suppressMessages({
                    object <- calculateMetrics(object, assay = 1L)
                })
            }
        }
        ## Plot.
        p <- do.call(
            what = .plotQCMetric,
            args = list(
                "object" = object,
                "metricCol" = "nCount",
                "geom" = geom,
                "interestingGroups" = interestingGroups,
                "min" = min,
                "max" = max,
                "trans" = trans
            )
        )
        ## Calculate barcode ranks and label inflection or knee points.
        if (!identical(point, "none")) {
            if (length(title)) {
                p <- p + labs(subtitle = paste(point, "point per sample"))
            }
            sampleNames <- sampleNames(object)
            ranks <- barcodeRanksPerSample(object)
            ## Inflection or knee points per sample.
            points <- lapply(
                X = ranks,
                FUN = function(x) {
                    assert(is(x, "DFrame"))
                    out <- metadata(x)[[point]]
                    assert(is.numeric(out))
                    out
                }
            )
            points <- unlist(points)
            names(points) <- names(ranks)
            assert(areSetEqual(names(sampleNames), names(points)))
            points <- points[names(sampleNames)]
            if (identical(geom, "ecdf")) {
                ## Calculate the y-intercept per sample.
                assert(isSubset("nCount", colnames(colData(object))))
                freq <- Map(
                    sampleId = names(points),
                    point = points,
                    MoreArgs = list(
                        "idCol" = matchSampleColumn(object),
                        "metrics" = metrics(object)
                    ),
                    f = function(metrics, sampleId, idCol, point) {
                        nCount <- metrics[
                            metrics[[idCol]] == sampleId,
                            "nCount",
                            drop = TRUE
                        ]
                        e <- ecdf(sort(nCount))
                        e(point)
                    }
                )
                freq <- unlist(freq, recursive = FALSE, use.names = FALSE)
                pointData <- data.frame(
                    "x" = points,
                    "y" = freq,
                    "label" = paste0(sampleNames, " (", points, ")"),
                    "sampleName" = sampleNames,
                    "interestingGroups" = sampleNames
                )
                p <- p +
                    geom_point(
                        data = pointData,
                        mapping = aes(
                            x = .data[["x"]],
                            y = .data[["y"]],
                            color = .data[["sampleName"]]
                        ),
                        size = 5L,
                        show.legend = FALSE
                    ) +
                    acid_geom_label_repel(
                        data = pointData,
                        mapping = aes(
                            x = .data[["x"]],
                            y = .data[["y"]],
                            label = .data[["label"]],
                            color = .data[["sampleName"]]
                        )
                    )
            }
        }
        ## Return.
        p <- p + labs(title = title)
        p
    }

formals(`plotCountsPerCell,SCE`)[["geom"]] <- # nolint
    .formalsList[["geom"]]



#' @rdname plotCountsPerCell
#' @export
setMethod(
    f = "plotCountsPerCell",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotCountsPerCell,SCE`
)
