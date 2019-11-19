#' UpSet plot
#'
#' S4 wrapper for [UpSetR::upset()] with improved default aesthetics.
#'
#' @name plotUpset
#' @inherit UpSetR::upset details params references
#' @note Updated 2019-11-19.
#'
#' @inheritParams acidroxygen::params
#' @param ... Handoff to [UpSetR::upset()].
#'
#' @return Graphical output, no return.
#'
#' @examples
#' ## Refer to 'UpSetR::upset()' documentation for more examples.
#' movies <- import(system.file("extdata", "movies.csv", package = "UpSetR"))
#' plotUpset(movies)
NULL



#' @rdname plotUpset
#' @name plotUpset
#' @importFrom bioverbs plotUpset
#' @usage plotUpset(object, ...)
#' @export
NULL



`plotUpset,data.frame` <-  # nolint
    function(
        object,
        ## nolint start
        line.size = 1L,
        main.bar.color = "black",
        matrix.color = "black",
        matrix.dot.alpha = 1L,
        mb.ratio = c(0.6, 0.4),
        point.size = 3L,
        sets.bar.color = "black",
        shade.alpha = 1L,
        shade.color = NA,
        text.scale = 1.5,
        ## Fix for T/F usage in UpSetR package.
        decreasing = c(TRUE, FALSE),
        keep.order = FALSE,
        ## nolint end
        ...
    ) {
        UpSetR::upset(
            data = object,
            line.size = line.size,
            main.bar.color = main.bar.color,
            matrix.color = matrix.color,
            matrix.dot.alpha = matrix.dot.alpha,
            mb.ratio = mb.ratio,
            point.size = point.size,
            sets.bar.color = sets.bar.color,
            shade.alpha = shade.alpha,
            shade.color = shade.color,
            text.scale = text.scale,
            decreasing = decreasing,
            keep.order = keep.order,
            ...
        )
    }



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
