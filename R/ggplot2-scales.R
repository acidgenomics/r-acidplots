#' Automatic color/fill scales to use with ggplot object
#'
#' Helper functions for setting default colors in Acid Genomics plots.
#'
#' @section Setting default colors:
#'
#' These functions will inherit values defined in global `options()`. Either
#' input a ggplot2 color scale function that returns `Scale` class (e.g.
#' `acid_scale_color_synesthesia_c`), or can input `"gradient"`, `"viridis"`
#' character strings.
#'
#' - `acid_scale_color_continuous()`: `"ggplot2.continuous.colour"`.
#' - `acid_scale_color_discrete()`: `"ggplot2.discrete.colour"`.
#' - `acid_scale_fill_continuous()`: `"ggplot2.continuous.fill"`.
#' - `acid_scale_fill_discrete()`: `"ggplot2.discrete.fill"`.
#'
#' @name autoScale
#' @note Updated 2023-08-11.
#'
#' @seealso
#' - https://ggplot2.tidyverse.org/reference/scale_colour_continuous.html
#' - https://ggplot2.tidyverse.org/reference/scale_colour_discrete.html
#'
#' @return `Scale`/`ggproto`/`gg`.
#'
#' @examples
#' x <- acid_scale_color_continuous()
#' class(x)
#' x <- acid_scale_color_discrete()
#' class(x)
#' x <- acid_scale_fill_continuous()
#' class(x)
#' x <- acid_scale_fill_discrete()
#' class(x)
NULL



#' @rdname autoScale
#' @export
acid_scale_color_continuous <- # nolint
    function() {
        x <- scale_colour_continuous(
            type = getOption(
                x = "ggplot2.continuous.colour",
                default = acid_scale_color_synesthesia_c
            )
        )
        assert(isGGScale(x, scale = "continuous", aes = "color"))
        x
    }



#' @rdname autoScale
#' @export
acid_scale_fill_continuous <- # nolint
    function() {
        x <- scale_fill_continuous(
            type = getOption(
                x = "ggplot2.continuous.fill",
                default = acid_scale_fill_synesthesia_c
            )
        )
        assert(isGGScale(x, scale = "continuous", aes = "fill"))
        x
    }



#' @rdname autoScale
#' @export
acid_scale_color_discrete <- # nolint
    function() {
        x <- scale_colour_discrete(
            type = getOption(
                x = "ggplot2.discrete.colour",
                default = acid_scale_color_synesthesia_d
            )
        )
        assert(isGGScale(x, scale = "discrete", aes = "color"))
        x
    }



#' @rdname autoScale
#' @export
acid_scale_fill_discrete <- # nolint
    function() {
        x <- scale_fill_discrete(
            type = getOption(
                x = "ggplot2.discrete.fill",
                default = acid_scale_fill_synesthesia_d
            )
        )
        assert(isGGScale(x, scale = "discrete", aes = "fill"))
        x
    }
