#' Flip coordinates
#'
#' Intelligently flip a plot with discrete data on the X axis.
#'
#' This function puts the samples that were near the left origin on the X at the
#' top on the Y axis, making them more human readable.
#'
#' @export
#' @note To my knowledge, there's not an easy way to create a `ggproto` object
#'   (via CoordFlip ggproto call internally) that lets you reorder the samples
#'   on the Y-axis to be reversed. So this function works directly on the
#'   `ggplot` object instead of a `ggproto`, and therefore doesn't currently
#'   support the `+` operator.
#' @note Updated 2020-03-11.
#'
#' @param object `ggplot`.
#'
#' @return `ggplot`.
#'
#' @seealso
#' - `coord_flip()`.
#' - `ggplot2::ggproto()`.
#' - `ggplot2:::add_ggplot`.
#' - `ggplot2:::+.gg`.
#' - https://stackoverflow.com/questions/40450904
#' - https://stackoverflow.com/questions/34227967
#' - https://cran.r-project.org/web/packages/ggplot2/vignettes/
#'       extending-ggplot2.html
#' - https://github.com/tidyverse/ggplot2/blob/master/R/ggproto.r
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(data = mpg, aes(x = class)) + geom_bar()
#'
#' ## Notice the difference in Y axis sample order.
#' g + coord_flip()
#' g <- acid_coord_flip(g)
acid_coord_flip <-  # nolint
    function(object) {
        assert(is(object, "ggplot"))
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



#' Remove Y axis padding
#'
#' @export
#' @note Updated 2019-07-29.
#'
#' @inheritParams ggplot2::scale_y_continuous
#'
#' @return `ggproto`.
#'
#' @examples
#' acid_scale_y_continuous_nopad()
acid_scale_y_continuous_nopad <-  # nolint
    function(
        ...,
        expand = ggplot2::expansion(mult = c(0L, 0.025))
    ) {
        scale_y_continuous(..., expand = expand)
    }
