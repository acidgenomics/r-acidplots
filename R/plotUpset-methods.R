## FIXME Need to improve working example to test coverage of
##       sortIntersections and sortSets.
## FIXME Set the minimum number of intersections to plot???
## FIXME Use `min_size` here to set the intersection size limit.
## FIXME Allow the user to set `maxSize`?



#' @name plotUpset
#' @inherit AcidGenerics::plotUpset
#' @note Updated 2021-09-09.
#'
#' @details
#' S4 wrapper for `ComplexUpset::upset()` with improved default aesthetics.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param nIntersections `integer(1)` or `Inf`.
#'   Maximum number of intersections to plot.
#'   Set `Inf` to plot all intersections.
#' @param orderBySize `logical`.
#'   Whether to order main bar plot and/or intersection matrix by set size.
#'
#'   - `"intersection"`: refers to main intersection bar plot.
#'   - `"sets"`: refers to set intersection matrix, shown as connected dots.
#'     When `TRUE`, orders by the set size (see plot to the left).
#'
#'   Can pass in `TRUE`/`FALSE` boolean flag and both `"intersection"` and
#'   `"sets"` settings will inherit.

#' @param ... Additional arguments.
#'
#' @seealso
#' - `intersectionMatrix()`.
#' - `ComplexUpset::upset()`.
#' - UpSetR package (legacy approach).
#'
#' @examples
#' list <- list(
#'     "aaa" = c("a", "b", "c", "d", "e", "f"),
#'     "bbb" = c("b", "c", "d", "e", "f", "g"),
#'     "ccc" = c("c", "d", "e", "f", "g", "h"),
#'     "ddd" = c("d", "e", "f", "g", "h", "i")
#' )
#' print(list)
#' mat <- intersectionMatrix(list)
#' print(mat)
#'
#' ## list ====
#' plotUpset(list)
#'
#' ## matrix ====
#' plotUpset(mat)
NULL



## Updated 2021-08-11.
`plotUpset,list` <- # nolint
    function(object, ...) {
        object <- intersectionMatrix(object)
        plotUpset(object, ...)
    }



## Updated 2021-09-09.
`plotUpset,matrix` <-  # nolint
    function(
        object,

        ## FIXME Think about the names of these arguments.
        minSize = 0L,
        maxSize = Inf,
        nIntersections = 40L,
        orderBySize = c(
            "intersections" = TRUE,
            "sets" = TRUE
        ),
        labels = list(
            "title" = NULL,
            "subtitle" = NULL
        )
    ) {
        whatPkg <- "ComplexUpset"
        whatFun <- "upset"
        requireNamespaces(whatPkg)
        if (is.logical(object)) {
            mode(object) <- "integer"
        }
        if (isFlag(orderBySize)) {
            orderBySize <- c(
                "intersections" = orderBySize,
                "sets" = orderBySize
            )
        }
        assert(
            is.integer(object),
            all(object %in% c(0L, 1L)),
            isNonNegative(minSize),
            isNonNegative(maxSize),
            isInt(nIntersections) || is.infinite(nIntersections),
            is.logical(orderBySize),
            areSetEqual(
                x = names(orderBySize),
                y = names(eval(formals()[["orderBySize"]]))
            )
        )
        if (!is.finite(nIntersections)) {
            nIntersections <- NULL
        }
        ## FIXME Can we rework this to handle like match.arg, where we
        ## don't have to specify the formals by default?
        labels <- matchLabels(labels)
        args <- list(
            "data" = as.data.frame(object),
            "intersect" = colnames(object),
            ## Label shown below the intersection matrix.
            "name" = NULL,

            "max_size" = maxSize,
            "min_size" = maxSize,

            ## The exact number of the intersections to be displayed;
            ## "n" largest intersections that meet criteria will be shown.
            "n_intersections" = nIntersections,
            ## Whether to sort the columns in the intersection matrix.
            "sort_intersections" = ifelse(
                test = orderBySize[["intersections"]],
                "yes" = "descending",
                "no" = FALSE
            ),
            ## Whether to sort the rows in the intersection matrix.
            "sort_sets" = ifelse(
                test = orderBySize[["sets"]],
                "yes" = "descending",
                "no" = FALSE
            )
        )
        what <- get(x = whatFun, envir = asNamespace(whatPkg), inherits = FALSE)
        assert(is.function(what))
        p <- do.call(what = what, args = args)
        p <- p + do.call(what = labs, args = labels)
        p
    }



## Updated 2021-09-08.
`plotUpset,data.frame` <-  # nolint
    function(object, ...) {
        keep <- bapply(
            X = object,
            FUN = function(x) {
                all(x %in% c(0L, 1L))
            }
        )
        assert(
            any(keep),
            msg = "Data frame does not contain any columns with 0, 1 values."
        )
        mat <- as.matrix(object[, keep, drop = FALSE])
        mode(mat) <- "integer"
        plotUpset(mat, ...)
    }



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

#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("data.frame"),
    definition = `plotUpset,data.frame`
)

#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("list"),
    definition = `plotUpset,list`
)

#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature("matrix"),
    definition = `plotUpset,matrix`
)
