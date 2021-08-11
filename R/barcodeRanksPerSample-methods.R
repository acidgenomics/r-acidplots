#' @name barcodeRanksPerSample
#' @inherit AcidGenerics::barcodeRanksPerSample
#' @note Requires DropletUtils package to be installed.
#' @note Updated 2019-11-19.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Passthrough arguments to `DropletUtils::barcodeRanks()`.
#'
#' @seealso
#' - `DropletUtils::barcodeRanks()`.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' if (requireNamespace("DropletUtils", quietly = TRUE)) {
#'     object <- SingleCellExperiment
#'     x <- barcodeRanksPerSample(object)
#'     names(x)
#' }
NULL



## nolint start
## Ensure we muffle this warning:
## > Warning in smooth.spline(x[new.keep], y[new.keep], df = df, ...) :
## > not using invalid df; must have 1 < df <= n := #{unique x} = 13
## > Calls: barcodeRanksPerSample ... lapply -> FUN -> do.call -> <Anonymous>
## > -> smooth.spline
## nolint end



## Updated 2021-08-11.
`barcodeRanksPerSample,SingleCellExperiment` <-  # nolint
    function(object, ...) {
        requireNamespaces("DropletUtils")
        counts <- counts(object)
        cell2sample <- cell2sample(object)
        samples <- levels(cell2sample)
        ## Subset the counts per sample into a list.
        countsPerSample <- lapply(
            X = samples,
            FUN = function(sample, counts) {
                cells <- names(cell2sample)[which(cell2sample == sample)]
                counts[, cells, drop = FALSE]
            },
            counts = counts
        )
        names(countsPerSample) <- samples
        ## Calculate the ranks per sample.
        ## Note that this now supports sparse matrices.
        DataFrameList(lapply(
            X = countsPerSample,
            FUN = function(counts) {
                x <- withCallingHandlers(
                    expr = {
                        DropletUtils::barcodeRanks(m = counts, ...)
                    },
                    warning = function(w) {
                        if (isTRUE(grepl(
                            pattern = "invalid df",
                            x = as.character(w)
                        ))) {
                            invokeRestart("muffleWarning")
                        } else {
                            w
                        }
                    }
                )
                ## Check DropletUtils return.
                assert(
                    is(x, "DataFrame"),
                    identical(
                        x = colnames(x),
                        y = c("rank", "total", "fitted")
                    ),
                    isSubset(
                        x = names(metadata(x)),
                        y = c("knee", "inflection")
                    )
                )
                x
            }
        ))
    }



#' @rdname barcodeRanksPerSample
#' @export
setMethod(
    f = "barcodeRanksPerSample",
    signature = signature("SingleCellExperiment"),
    definition = `barcodeRanksPerSample,SingleCellExperiment`
)
