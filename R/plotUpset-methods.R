#' UpSet plot
#'
#' S4 wrapper for [UpSetR::upset()] with improved default aesthetics.
#'
#' @name plotUpset
#' @note Updated 2020-08-05.
#'
#' @inheritParams acidroxygen::params
#' @param sets `character` or `NULL`.
#'   Specific sets to include in plot.
#'   If left `NULL`, all sets will be included.
#' @param orderBySize `logical`.
#'   Whether to order matrix and/or main bar plot by size.
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



## Updated 2020-08-25.
`plotUpset,matrix` <-  # nolint
    function(
        object,
        sets = NULL,
        orderBySize = c(matrix = TRUE, mainBar = TRUE)
    ) {
        assert(
            is.logical(orderBySize),
            areSetEqual(
                x = names(orderBySize),
                y = names(eval(formals()[["orderBySize"]]))
            )
        )
        if (!is.null(sets)) {
            assert(isSubset(sets, colnames(object)))
        }
        args <- list(
            data = as.data.frame(object),
            decreasing = TRUE,
            keep.order = !isTRUE(orderBySize[["mainBar"]]),
            line.size = 1L,
            main.bar.color = "black",
            matrix.color = "black",
            matrix.dot.alpha = 1L,
            mb.ratio = c(0.5, 0.5),
            nintersects = NA,
            nsets = ncol(object),
            order.by = ifelse(
                test = isTRUE(orderBySize[["matrix"]]),
                yes = "freq",
                no = "degree"
            ),
            point.size = 3L,
            sets = sets,
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
