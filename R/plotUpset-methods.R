#' UpSet plot
#'
#' S4 wrapper for [UpSetR::upset()] with improved default aesthetics.
#'
#' @name plotUpset
#' @note Updated 2020-07-23.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @return Graphical output, no return.
#'
#' @examples
#' list <- list(
#'     a = c("a", "b", "c", "d", "e", "f"),
#'     b = c("b", "c", "d", "e", "f", "g"),
#'     c = c("c", "d", "e", "f", "g", "h"),
#'     d = c("d", "e", "f", "g", "h", "i")
#' )
#' plotUpset(list)
NULL



#' @rdname plotUpset
#' @name plotUpset
#' @importFrom acidgenerics plotUpset
#' @usage plotUpset(object, ...)
#' @export
NULL



## Modified version of `UpSetR::fromList()`.
## Updated 2020-07-23.
.upsetMatrixFromList <- function(list) {
    elements <- unique(unlist(list))
    data <- unlist(lapply(list, function(x) {
        x <- as.vector(match(elements, x))
    }))
    data[is.na(data)] <- as.integer(0L)
    data[data != 0L] <- as.integer(1L)
    data <- matrix(data, ncol = length(list), byrow = FALSE)
    data <- data[which(rowSums(data) != 0L), , drop = FALSE]
    colnames(data) <- names(list)
    data
}



## Updated 2020-07-23.
`plotUpset,list` <- # nolint
    function(object, ...) {
        plotUpset(.upsetMatrixFromList(object))
    }



#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("list"),
    definition = `plotUpset,list`
)



## Updated 2020-07-23.
`plotUpset,matrix` <-  # nolint
    function(object) {
        args <- list(
            data = as.data.frame(object),
            decreasing = TRUE,
            keep.order = FALSE,
            line.size = 1L,
            main.bar.color = "black",
            matrix.color = "black",
            matrix.dot.alpha = 1L,
            mb.ratio = c(0.7, 0.3),
            nintersects = NA,
            nsets = ncol(object),
            order.by = "freq",
            point.size = 3L,
            sets.bar.color = "black",
            shade.alpha = 1L,
            shade.color = NA,
            text.scale = 1.5
        )
        do.call(what = UpSetR::upset, args = args)
    }



#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("matrix"),
    definition = `plotUpset,matrix`
)



## Updated 2020-07-23.
`plotUpset,data.frame` <-  # nolint
    `plotUpset,matrix`



#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("data.frame"),
    definition = `plotUpset,data.frame`
)



## Updated 2020-07-23.
`plotUpset,DataFrame` <-  # nolint
    `plotUpset,data.frame`



#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("DataFrame"),
    definition = `plotUpset,DataFrame`
)
