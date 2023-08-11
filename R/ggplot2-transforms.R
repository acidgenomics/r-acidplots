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
#'
#' ## Notice the difference in Y axis sample order.
#' g + coord_flip()
#' g + acid_coord_flip()
acid_coord_flip <- # nolint
    function() {
        list(
            scale_x_discrete(limits = rev),
            coord_flip()
        )
    }



## FIXME Need a working example here.

#' Remove Y axis padding
#'
#' @export
#' @note Updated 2021-06-29.
#'
#' @param expand `numeric`.
#' Range expansion.
#' Refer to `ggplot2::expansion` for details.
#'
#' @param ... Other arguments passed to `scale_y_continuous()`.
#'
#' @return `ggproto`.
#'
#' @examples
#' acid_scale_y_continuous_nopad()
acid_scale_y_continuous_nopad <- # nolint
    function(...,
             expand = ggplot2::expansion(mult = c(0L, 0.025))) {
        scale_y_continuous(..., expand = expand)
    }
