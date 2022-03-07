#' @name plotKnownMarkers
#' @inherit AcidGenerics::plotKnownMarkers
#' @note Updated 2022-03-05.
#'
#' @inheritParams AcidRoxygen::params
#' @param markers Object.
#' @param ... Passthrough arguments to [plotMarker()].
#'
#' @examples
#' data(
#'     KnownMarkers,
#'     SingleCellExperiment_Seurat,
#'     package = "AcidTest"
#' )
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_Seurat
#' markers <- KnownMarkers
#' plotKnownMarkers(
#'     object = object,
#'     markers = markers,
#'     reduction = "UMAP"
#' )
NULL



## Updated 2019-09-03.
`plotKnownMarkers,SCE,KnownMarkers` <-  # nolint
    function(
        object,
        markers,
        reduction,
        headerLevel,
        ...
    ) {
        validObject(object)
        validObject(markers)
        ## Handle gene identifier to symbol conversion automatically.
        if (
            !isSubset(
                x = unique(markers[["name"]]),
                y = rownames(object)
            ) &&
            isSubset(
                x = unique(markers[["name"]]),
                y = unique(decode(rowData(object)[["geneName"]]))
            )
        ) {
            object <- convertGenesToSymbols(object)
        }
        assert(
            isSubset(
                x = unique(markers[["name"]]),
                y = rownames(object)
            ),
            isScalar(reduction),
            isHeaderLevel(headerLevel)
        )
        markers <- as(markers, "DataFrame")
        cellTypes <- markers[["cellType"]]
        cellTypes <- unique(na.omit(as.character(cellTypes)))
        assert(hasLength(cellTypes))
        list <- lapply(
            X = cellTypes,
            FUN = function(cellType) {
                genes <- markers[
                    markers[["cellType"]] == cellType,
                    "name",
                    drop = TRUE
                ]
                genes <- unique(na.omit(as.character(genes)))
                assert(hasLength(genes))
                markdownHeader(
                    text = cellType,
                    level = headerLevel,
                    tabset = TRUE,
                    asis = TRUE
                )
                lapply(genes, function(gene) {
                    p <- plotMarker(
                        object = object,
                        genes = gene,
                        reduction = reduction,
                        ...
                    )
                    show(p)
                    invisible(p)
                })
            }
        )
        invisible(list)
    }

formals(`plotKnownMarkers,SCE,KnownMarkers`)[
    c(
        "BPPARAM",
        "headerLevel",
        "reduction"
    )] <-
    .formalsList[c(
        "BPPARAM",
        "headerLevel",
        "reduction"
    )]



#' @rdname plotKnownMarkers
#' @export
setMethod(
    f = "plotKnownMarkers",
    signature = signature(
        object = "SingleCellExperiment",
        markers = "KnownMarkers"
    ),
    definition = `plotKnownMarkers,SCE,KnownMarkers`
)
