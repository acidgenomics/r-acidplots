#' Flip discrete data
#'
#' Intelligently flip a plot with discrete data on the X axis.
#'
#' This function puts the samples that were near the left origin on the X at the
#' top on the Y axis, making them more human readable.
#'
#' @param object `ggplot`.
#' @param xCol `character(1)`.
#'   Column name of discrete data plotted on the X axis.
#'
#' @return `ggplot`.
#'
#' @seealso
#' - `coord_flip()`.
#'
#' @examples
#' g <- ggplot(data = ggplot2::mpg, aes(x = class)) + geom_bar()
#' print(g)
#' flip_x_discrete(g)
flip_x_discrete <- function(object) {
    assert(
        is(object, "ggplot"),
        isSubset(
            x = "x",
            y = object[["layers"]][[1L]][["geom"]][["required_aes"]]
        )
    )

    data <- object[["data"]]
    assert(is.data.frame(data))

    mapping <- .detectMapping(object)
    assert(is(mapping, "uneval"))
    xCol <- quo_text(mapping[["x"]])
    limits <- rev(levels(as.factor(data[[xCol]])))

    object +
        scale_x_discrete(limits = limits) +
        coord_flip()
}
