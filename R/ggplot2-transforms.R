#' Flip coordinates
#'
#' Intelligently flip a plot with discrete data on the X axis.
#'
#' This function puts the samples that were near the left origin on the X at the
#' top on the Y axis, making them more human readable.
#'
#' @export
#'
#' @param object `ggplot`.
#'
#' @return `ggplot`.
#'
#' @seealso
#' - `coord_flip()`.
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(data = mpg, aes(x = class)) + geom_bar()
#' print(g)
#' acid_coord_flip(g)
acid_coord_flip <-  # nolint
    function(object) {
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



#' Remove Y axis padding
#' @export
#' @inheritParams ggplot2::scale_y_continuous
#' @examples
#' acid_scale_y_continuous_nopad()
acid_scale_y_continuous_nopad <-  # nolint
    function(
        ...,
        expand = expand_scale(mult = c(0L, 0.025))
    ) {
        scale_y_continuous(..., expand = expand)
    }
