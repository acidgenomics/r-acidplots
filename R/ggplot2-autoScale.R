## FIXME Check handling in code coverage if "viridis" is set as default.



#' Automatic color/fill scales to use with ggplot object
#'
#' Helper functions for setting default colors in Acid Genomics plots.
#'
#' @name autoScale
#' @note Updated 2021-09-09.
#'
#' @return `Scale`/`ggproto`/`gg`.
#'
#' @examples
#' x <- autoContinuousColorScale()
#' class(x)
#'
#' x <- autoContinuousFillScale()
#' class(x)
#'
#' x <- autoDiscreteColorScale()
#' class(x)
#'
#' x <- autoDiscreteFillScale()
#' class(x)
NULL



#' @rdname autoScale
#' @export
autoContinuousColorScale <- function() {
    scale_colour_continuous(
        type = getOption(
            x = "ggplot2.continuous.colour",
            default = scale_color_synesthesia_c
        )
    )
}



#' @rdname autoScale
#' @export
autoContinuousFillScale <- function() {
    scale_fill_continuous(
        type = getOption(
            x = "ggplot2.continuous.fill",
            default = scale_fill_synesthesia_c
        )
    )
}



#' @rdname autoScale
#' @export
autoDiscreteColorScale <- function() {
    scale_colour_discrete(
        type = getOption(
            x = "ggplot2.discrete.colour",
            default = scale_color_synesthesia_d
        )
    )
}



#' @rdname autoScale
#' @export
autoDiscreteFillScale <- function() {
    scale_fill_discrete(
        type = getOption(
            x = "ggplot2.discrete.fill",
            default = scale_fill_synesthesia_d
        )
    )
}
