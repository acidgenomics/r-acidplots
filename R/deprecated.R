## nocov start
## nolint start



#' @name defunct
#' @inherit basejump::defunct
#' @keywords internal
NULL



#' @name deprecated
#' @inherit basejump::deprecated
#' @keywords internal
NULL



## v0.2.0 =======================================================================
#' @rdname defunct
#' @export
plotCountsPerGene <- function(...) {
    .Defunct("plotCountsPerFeature")
}

#' @rdname defunct
#' @export
plotGenesDetected <- function(...) {
    .Defunct("plotFeaturesDetected")
}



## nocov end
## nolint end
