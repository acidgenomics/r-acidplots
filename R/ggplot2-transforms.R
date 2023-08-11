#' Flip coordinates
#'
#' Intelligently flip a plot with discrete data on the X axis.
#'
#' @export
#' @note Updated 2023-08-11.
#'
#' @details
#' This function puts the samples that were near the left origin on the X at the
#' top on the Y axis, making them more human readable.
#'
#' @return `list` of `ggproto` objects.
#'
#' @seealso
#' - `coord_flip()`.
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(data = mpg, aes(x = class)) + geom_bar()
#' ## Notice the difference in Y axis sample order.
#' g + coord_flip()
#' g + acid_discrete_coord_flip()
acid_discrete_coord_flip <- # nolint
    function() {
        list(
            scale_x_discrete(limits = rev),
            coord_flip()
        )
    }



#' Remove Y axis padding
#'
#' @export
#' @note Updated 2023-08-11.
#'
#' @return `ggproto`.
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(data = mpg, aes(x = class)) + geom_bar()
#' ## By default, plots contain padding at the top of the graph.
#' print(g)
#' ## This function will automatically remove the padding.
#' g + acid_scale_y_continuous_nopad()
acid_scale_y_continuous_nopad <- # nolint
    function() {
        scale_y_continuous(expand = expansion(mult = c(0L, 0L)))
    }
