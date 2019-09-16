#' @name plotGenderMarkers
#' @inherit bioverbs::plotGenderMarkers
#' @note Currently only *Homo sapiens* and *Mus musculus* genomes are supported.
#' @note Updated 2019-09-15.
#'
#' @inheritParams acidroxygen::params
#' @param ... Passthrough to [plotCounts()], with the `genes` argument
#'   automatically defined.
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



## Updated 2019-09-15.
`plotGenderMarkers,SummarizedExperiment` <-  # nolint
    function(object, style = "wide", ...) {
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
        plotCounts(
            object = object,
            genes = genes,
            style = style,
            ...
        )
    }



#' @rdname plotGenderMarkers
#' @export
setMethod(
    f = "plotGenderMarkers",
    signature = signature("SummarizedExperiment"),
    definition = `plotGenderMarkers,SummarizedExperiment`
)



## Updated 2019-09-15.
`plotGenderMarkers,SingleCellExperiment` <-  # nolint
    function(object, ...) {
        plotGenderMarkers(
            object = pseudobulk(object),
            ...
        )
    }



#' @describeIn plotGenderMarkers Applies [pseudobulk()] calculation to average
#'   gene expression at sample level prior to plotting.\cr
#'   Passes `...` to `SummarizedExperiment` method.
#' @export
setMethod(
    f = "plotGenderMarkers",
    signature = signature("SingleCellExperiment"),
    definition = `plotGenderMarkers,SingleCellExperiment`
)
