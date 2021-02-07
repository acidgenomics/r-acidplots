#' @name plotGenderMarkers
#' @inherit AcidGenerics::plotGenderMarkers
#' @note Currently only *Homo sapiens* and *Mus musculus* genomes are supported.
#' @note Updated 2021-02-07.
#'
#' @inheritParams plotCounts
#' @inheritParams AcidRoxygen::params
#' @param ... Passthrough to [plotCounts()], with the `genes` argument
#'   automatically defined.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
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



## Updated 2021-02-07.
`plotGenderMarkers,SummarizedExperiment` <-  # nolint
    function(object, style = "wide", ...) {
        ## Load the relevant internal gender markers data.
        organism <- organism(object)
        assert(isString(organism))
        organism <- camelCase(organism, strict = TRUE)
        data(
            list = "genderMarkers",
            package = packageName(),
            envir = environment()
        )
        markers <- get("genderMarkers", inherits = FALSE)
        assert(
            is.list(markers),
            isSubset(organism, names(markers))
        )
        gr <- markers[[organism]]
        assert(
            is(gr, "GRanges"),
            isSubset("geneId", names(mcols(gr)))
        )
        ## NOTE We're allowing mapping of genes without a perfect identifier
        ## version match here (e.g. ENSG00000012817 to ENSG00000012817.16).
        genes <- mapGenesToRownames(
            object = object,
            genes = sort(decode(mcols(gr)[["geneId"]])),
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



## Updated 2020-02-19.
`plotGenderMarkers,SingleCellExperiment` <-  # nolint
    function(object, ...) {
        plotGenderMarkers(
            object = aggregateCellsToSamples(object),
            ...
        )
    }



#' @rdname plotGenderMarkers
#' @export
setMethod(
    f = "plotGenderMarkers",
    signature = signature("SingleCellExperiment"),
    definition = `plotGenderMarkers,SingleCellExperiment`
)
