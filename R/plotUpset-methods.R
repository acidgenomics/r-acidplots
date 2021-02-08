## NOTE Consider migrating to ComplexUpset
##      (https://github.com/krassowski/complex-upset),
##      which uses ggplot internally instead.



#' UpSet plot
#'
#' S4 wrapper for `UpSetR::upset()` with improved default aesthetics.
#'
#' @name plotUpset
#' @note Updated 2021-02-08.
#'
#' @inheritParams AcidRoxygen::params
#' @param orderBySize `logical`.
#'   Whether to order main bar plot and/or intersection matrix by set size.
#'
#'   - "bars" refers to main bar plot.
#'   - "matrix" refers to intersection matrix, shown as connected dots.
#'
#'   Can pass in `TRUE`/`FALSE` boolean flag and both "bars" and "matrix"
#'   settings will inherit.
#' @param nIntersects `integer(1)` or `Inf`.
#'   Maximum number of intersections to plot.
#'   Set `Inf` to plot all intersections.
#' @param ... Additional arguments.
#'
#' @return Graphical output, no return.
#'
#' @seealso `upsetMatrix()`.
#'
#' @examples
#' list <- list(
#'     "aaa" = c("a", "b", "c", "d", "e", "f"),
#'     "bbb" = c("b", "c", "d", "e", "f", "g"),
#'     "ccc" = c("c", "d", "e", "f", "g", "h"),
#'     "ddd" = c("d", "e", "f", "g", "h", "i")
#' )
#' plotUpset(list)
NULL



## Updated 2021-02-08.
`plotUpset,list` <- # nolint
    function(object, ...) {
        plotUpset(
            object = intersectionMatrix(object),
            ...
        )
    }



#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("list"),
    definition = `plotUpset,list`
)



## Updated 2021-02-08.
`plotUpset,matrix` <-  # nolint
    function(
        object,
        nIntersects = 40L,
        orderBySize = c(bars = TRUE, matrix = TRUE)
    ) {
        requireNamespaces("UpSetR")
        if (is.logical(object)) {
            mode(object) <- "integer"
        }
        if (isFlag(orderBySize)) {
            orderBySize <- c("bars" = orderBySize, "matrix" = orderBySize)
        }
        assert(
            is.integer(object),
            all(object %in% c(0L, 1L)),
            isInt(nIntersects) ||
                is.infinite(nIntersects) ||
                is.na(nIntersects),
            is.logical(orderBySize),
            areSetEqual(
                x = names(orderBySize),
                y = names(eval(formals()[["orderBySize"]]))
            )
        )
        if (!is.finite(nIntersects)) nIntersects <- NA
        args <- list(
            data = as.data.frame(object),
            keep.order = !isTRUE(orderBySize[["matrix"]]),
            line.size = 1L,
            main.bar.color = "black",
            matrix.color = "black",
            matrix.dot.alpha = 1L,
            mb.ratio = c(0.5, 0.5),
            nintersects = nIntersects,
            nsets = ncol(object),
            point.size = 3L,
            sets = rev(colnames(object)),
            sets.bar.color = "black",
            shade.alpha = 1L,
            shade.color = NA,
            text.scale = 1.5
        )
        if (isTRUE(orderBySize[["bars"]])) {
            args[["order.by"]] <- "freq"
            args[["decreasing"]] <- TRUE
        } else {
            args[["order.by"]] <- c("freq", "degree")
            args[["decreasing"]] <- c(TRUE, FALSE)
        }
        suppressMessages({
            do.call(what = UpSetR::upset, args = args)
        })
    }



#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("matrix"),
    definition = `plotUpset,matrix`
)



## Updated 2020-08-25.
`plotUpset,data.frame` <-  # nolint
    function(object, ...) {
        keep <- bapply(X = object, FUN = function(x) all(x %in% c(0L, 1L)))
        if (!any(keep)) {
            stop("Data frame does not contain any columns with 0, 1 values.")
        }
        mat <- as.matrix(object[, keep, drop = FALSE])
        mode(mat) <- "integer"
        plotUpset(mat, ...)
    }



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
