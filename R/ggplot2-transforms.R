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
#' To my knowledge, there's not an easy way to create a `ggproto` object
#' (via CoordFlip ggproto call internally) that lets you reorder the samples
#' on the Y-axis to be reversed. So this function works directly on the
#' `ggplot` object instead of a `ggproto`, and therefore doesn't currently
#' support the `+` operator.
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
#' - https://cran.r-project.org/web/packages/ggplot2/vignettes/
#' extending-ggplot2.html
#' - https://stackoverflow.com/questions/40450904
#' - https://stackoverflow.com/questions/34227967
#' - https://github.com/tidyverse/ggplot2/blob/master/R/ggproto.r
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(data = mpg, aes(x = class)) +
#'     geom_bar()
#'
#' ## Notice the difference in Y axis sample order.
#' g + coord_flip()
#' g <- acid_coord_flip(g)
acid_coord_flip <- # nolint
    function(object) {
        assert(is(object, "ggplot"))
        data <- object[["data"]]
        assert(is.data.frame(data))
        mapping <- .detectMapping(object)
        assert(
            is(mapping, "uneval"),
            isSubset("x", names(mapping)),
            is(mapping[["x"]], "quosure")
        )
        quo <- mapping[["x"]]
        if (quo_is_symbol(quo)) {
            xCol <- quo_text(quo)
        } else if (quo_is_symbolic(quo)) {
            pronoun <- quo_text(quo)
            pattern <- "^\\.data\\[\\[\"(.+)\"\\]\\]$"
            assert(isMatchingRegex(pattern = pattern, x = pronoun))
            xCol <- sub(pattern = pattern, replacement = "\\1", x = pronoun)
        } else {
            abort("Unexpected rlang quosure error.")
        }
        assert(
            isString(xCol),
            isSubset(xCol, colnames(data))
        )
        limits <- rev(levels(as.factor(data[[xCol]])))
        object +
            scale_x_discrete(limits = limits) +
            coord_flip()
    }



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
