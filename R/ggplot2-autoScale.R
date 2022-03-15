#' Automatic color/fill scales to use with ggplot object
#'
#' Helper functions for setting default colors in Acid Genomics plots.
#'
#' @section Setting default colors:
#'
#' These functions will inherit values defined in global `options()`. Either
#' input a ggplot2 color scale function that returns `Scale` class
#' (e.g. `scale_color_synesthesia_c`), or can input `"gradient"`, `"viridis"`
#' character strings.
#'
#' - `autoContinuousColorScale()`: `"ggplot2.continuous.colour"`.
#' - `autoContinuousFillScale()`: `"ggplot2.continuous.fill"`.
#' - `autoDiscreteColorScale()`: `"ggplot2.discrete.colour"`.
#' - `autoDiscreteFillScale()`: `"ggplot2.discrete.fill"`.
#'
#' @name autoScale
#' @note Updated 2021-09-10.
#'
#' @seealso
#' - https://ggplot2.tidyverse.org/reference/scale_colour_continuous.html
#' - https://ggplot2.tidyverse.org/reference/scale_colour_discrete.html
#'
#' @return `Scale`/`ggproto`/`gg`.
#'
#' @examples
#' x <- autoContinuousColorScale()
#' class(x)
#' x <- autoContinuousFillScale()
#' class(x)
#' x <- autoDiscreteColorScale()
#' class(x)
#' x <- autoDiscreteFillScale()
#' class(x)
NULL



#' @rdname autoScale
#' @export
autoContinuousColorScale <- function() {
    x <- scale_colour_continuous(
        type = getOption(
            x = "ggplot2.continuous.colour",
            default = scale_color_synesthesia_c
        )
    )
    assert(isGGScale(x, scale = "continuous", aes = "color"))
    x
}



#' @rdname autoScale
#' @export
autoContinuousFillScale <- function() {
    x <- scale_fill_continuous(
        type = getOption(
            x = "ggplot2.continuous.fill",
            default = scale_fill_synesthesia_c
        )
    )
    assert(isGGScale(x, scale = "continuous", aes = "fill"))
    x
}



#' @rdname autoScale
#' @export
autoDiscreteColorScale <- function() {
    x <- scale_colour_discrete(
        type = getOption(
            x = "ggplot2.discrete.colour",
            default = scale_color_synesthesia_d
        )
    )
    assert(isGGScale(x, scale = "discrete", aes = "color"))
    x
}



#' @rdname autoScale
#' @export
autoDiscreteFillScale <- function() {
    x <- scale_fill_discrete(
        type = getOption(
            x = "ggplot2.discrete.fill",
            default = scale_fill_synesthesia_d
        )
    )
    assert(isGGScale(x, scale = "discrete", aes = "fill"))
    x
}
