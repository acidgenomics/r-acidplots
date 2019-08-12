#' @name barcodeRanksPerSample
#' @inherit bioverbs::barcodeRanksPerSample
#' @inherit DropletUtils::barcodeRanks
#' @note Updated 2019-08-12.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @seealso [DropletUtils::barcodeRanks()].
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#'
#' ## SingleCellExperiment ====
#' if (packageVersion("DropletUtils") >= "1.4") {
#'     object <- SingleCellExperiment
#'     x <- barcodeRanksPerSample(object)
#'     names(x)
#' }
NULL



#' @rdname barcodeRanksPerSample
#' @name barcodeRanksPerSample
#' @importFrom bioverbs barcodeRanksPerSample
#' @usage barcodeRanksPerSample(object, ...)
#' @export
NULL



## nolint start
## Muffle this warning:
## > Warning in smooth.spline(x[new.keep], y[new.keep], df = df, ...) :
## > not using invalid df; must have 1 < df <= n := #{unique x} = 13
## > Calls: barcodeRanksPerSample ... lapply -> FUN -> do.call -> <Anonymous>
## > -> smooth.spline
## nolint end



## Updated 2019-08-12.
`barcodeRanksPerSample,SingleCellExperiment` <-  # nolint
    function(object) {
        assert(packageVersion("DropletUtils") >= "1.4")
        which <- sys.parent()
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
                    expr = do.call(
                        what = barcodeRanks,
                        args = matchArgsToDoCall(
                            args = list(m = counts),
                            removeFormals = "object",
                            which = which
                        )
                    ),
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

f1 <- formals(`barcodeRanksPerSample,SingleCellExperiment`)
f2 <- formals(barcodeRanks)
f2 <- f2[setdiff(names(f2), c(names(f1), "m", "..."))]
f <- c(f1, f2)
formals(`barcodeRanksPerSample,SingleCellExperiment`) <- f



#' @rdname barcodeRanksPerSample
#' @export
setMethod(
    f = "barcodeRanksPerSample",
    signature = signature("SingleCellExperiment"),
    definition = `barcodeRanksPerSample,SingleCellExperiment`
)
