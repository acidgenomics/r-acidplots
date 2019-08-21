#' @name plotGenderMarkers
#' @inherit bioverbs::plotGenderMarkers
#' @note Updated 2019-08-21.
#'
#' @inheritParams plotCounts
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @note Currently only *Homo sapiens* and *Mus musculus* genomes are supported.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotGenderMarkers(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotGenderMarkers(object)
NULL



#' @rdname plotGenderMarkers
#' @name plotGenderMarkers
#' @importFrom bioverbs plotGenderMarkers
#' @usage plotGenderMarkers(object, ...)
#' @export
NULL



## Updated 2019-08-21.
`plotGenderMarkers,SummarizedExperiment` <-  # nolint
    function() {
        validObject(object)
        ## Load the relevant internal gender markers data.
        organism <- organism(object)
        data(
            list = "genderMarkers",
            package = "acidplots",
            envir = environment()
        )
        markers <- get("genderMarkers", inherits = FALSE)
        assert(is.list(markers))
        supported <- names(markers)
        supported <- snakeCase(supported)
        supported <- sub("^([a-z])", "\\U\\1", supported, perl = TRUE)
        supported <- sub("_", " ", supported)
        ## Error if the organism is not supported.
        if (!isSubset(organism, supported)) {
            stop(sprintf(
                "'%s' is not supported.\nSupported: %s.",
                organism, toString(supported)
            ))
        }
        markers <- markers[[camelCase(organism)]]
        assert(is(markers, "tbl_df"))
        genes <- mapGenesToRownames(
            object = object,
            genes = markers[["geneID"]],
            strict = FALSE
        )
        do.call(
            what = plotCounts,
            args = matchArgsToDoCall(
                args = list(genes = genes)
            )
        )
    }

f <- formals(`plotCounts,SummarizedExperiment`)
f <- f[setdiff(names(f), "genes")]
f[["style"]] <- "wide"
formals(`plotGenderMarkers,SummarizedExperiment`) <- f



#' @rdname plotGenderMarkers
#' @export
setMethod(
    f = "plotGenderMarkers",
    signature = signature("SummarizedExperiment"),
    definition = `plotGenderMarkers,SummarizedExperiment`
)



## Updated 2019-08-21.
`plotGenderMarkers,SingleCellExperiment` <-  # nolint
    function(object) {
        object <- pseudobulk(object)
        do.call(
            what = plotGenderMarkers,
            args = matchArgsToDoCall(
                args = list(object = object)
            )
        )
    }

formals(`plotGenderMarkers,SingleCellExperiment`) <-
    formals(`plotGenderMarkers,SummarizedExperiment`)



#' @rdname plotGenderMarkers
#' @export
setMethod(
    f = "plotGenderMarkers",
    signature = signature("SingleCellExperiment"),
    definition = `plotGenderMarkers,SingleCellExperiment`
)
