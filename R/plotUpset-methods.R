#' UpSet plot
#'
#' S4 wrapper for [UpSetR::upset()] with improved default aesthetics.
#'
#' @name plotUpset
#' @note Updated 2020-07-22.
#'
#' @inheritParams acidroxygen::params
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
#' df <- UpSetR::fromList(list)
#' plotUpset(df)
NULL



#' @rdname plotUpset
#' @name plotUpset
#' @importFrom acidgenerics plotUpset
#' @usage plotUpset(object, ...)
#' @export
NULL



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



`plotUpset,data.frame` <-  # nolint
    `plotUpset,matrix`



#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("data.frame"),
    definition = `plotUpset,data.frame`
)



`plotUpset,DataFrame` <-  # nolint
    `plotUpset,data.frame`



#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("DataFrame"),
    definition = `plotUpset,DataFrame`
)
