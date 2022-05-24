## NOTE Is there a way to hide "Intersection size" text in the plot?
## FIXME Need to harden against empty list input.



#' @name plotUpset
#' @inherit AcidGenerics::plotUpset
#' @note Updated 2022-04-05.
#'
#' @details
#' S4 wrapper for `ComplexUpset::upset()` with improved default aesthetics.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param minSize,maxSize Non-negative `integer(1)` or `Inf`.
#' Minimal/maximal number of observations in an intersection for it to be
#' included. Defaults to all observations. Note that `maxSize` must be
#' greater than `minSize`.
#'
#' @param nIntersections `integer(1)` or `Inf`.
#' Maximum number of intersections to plot.
#' Set `Inf` to plot all intersections.
#'
#' @param orderBySize `logical`.
#' Whether to order main bar plot and/or intersection matrix by set size.
#'
#' - `"intersection"`: refers to main intersection bar plot.
#' - `"sets"`: refers to set intersection matrix, shown as connected dots.
#' When `TRUE`, orders by the set size (see plot to the left).
#'
#' Can pass in `TRUE`/`FALSE` boolean flag and both `"intersection"` and
#' `"sets"` settings will inherit.
#'
#' @seealso
#' - `intersectionMatrix()`.
#' - `ComplexUpset::upset()`.
#' - `ComplexUpset::upset_themes`.
#' - UpSetR package (legacy approach).
#'
#' @examples
#' ## list ====
#' list <- list(
#'     "aaa" = c("a", "b", "c", "d", "e", "f"),
#'     "bbb" = c("b", "c", "d", "e", "f", "g"),
#'     "ccc" = c("c", "d", "e", "f", "g", "h"),
#'     "ddd" = c("d", "e", "f", "g", "h", "i")
#' )
#' print(list)
#' plotUpset(list)
#'
#' ## matrix ====
#' mat <- AcidBase::intersectionMatrix(list)
#' print(mat)
#' plotUpset(mat)
NULL



## Updated 2021-08-11.
`plotUpset,list` <- # nolint
    function(object, ...) {
        object <- intersectionMatrix(object)
        plotUpset(object, ...)
    }



## Updated 2022-04-05.
`plotUpset,matrix` <- # nolint
    function(object,
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
             )) {
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
            allAreNonNegative(c(minSize, maxSize)),
            isTRUE(maxSize > minSize),
            isInt(nIntersections) || is.infinite(nIntersections),
            isPositive(nIntersections),
            is.logical(orderBySize),
            areSetEqual(
                x = names(orderBySize),
                y = names(eval(formals()[["orderBySize"]]))
            )
        )
        if (!is.finite(nIntersections)) {
            nIntersections <- NULL
        }
        args <- list(
            "data" = as.data.frame(object),
            "base_annotations" = list(
                "Intersection size" =
                    ComplexUpset::intersection_size(
                        counts = FALSE,
                        color = NA,
                        fill = "black"
                    )
            ),
            "intersect" = colnames(object),
            "max_size" = maxSize,
            "min_size" = minSize,
            ## This will define label shown below the intersection matrix.
            "name" = NULL,
            ## The exact number of the intersections to be displayed;
            ## "n" largest intersections that meet criteria will be shown.
            "n_intersections" = nIntersections,
            "set_sizes" = ComplexUpset::upset_set_size(
                geom = geom_bar(
                    width = 0.6,
                    color = NA,
                    fill = "black"
                )
            ),
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
`plotUpset,data.frame` <- # nolint
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



## Updated 2021-10-13.
`plotUpset,DataFrame` <- # nolint
    `plotUpset,data.frame`



#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature(object = "DataFrame"),
    definition = `plotUpset,DataFrame`
)

#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature(object = "data.frame"),
    definition = `plotUpset,data.frame`
)

#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature(object = "list"),
    definition = `plotUpset,list`
)

#' @rdname plotUpset
#' @export
setMethod(
    f = "plotUpset",
    signature = signature(object = "matrix"),
    definition = `plotUpset,matrix`
)
