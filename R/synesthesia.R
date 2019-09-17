#' Synesthesia color palette
#'
#' Four color gradient defined by purple, blue, green, orange.
#'
#' A little bit of an acid trip, especially when used for heatmaps.
#'
#' @name synesthesia
#' @note Updated 2019-09-13.
#'
#' @inheritParams acidroxygen::params
#' @param guide `character(1)` or `function`.
#'   A function used to create a guide or its name.
#'   See [`guides()`][ggplot2::guides] for details.
#' @param n `integer(1)`.
#'   The number of colors (>= 1) to be in the palette.
#' @param na.value `character(1)`.
#'   Missing values will be replaced with this value.
#' @param palette `character(1)`.
#'   Color palette name.
#'
#' @return `character` or `ggproto`.
#'
#' @seealso
#' Adapted from viridis color palette:
#'
#' - `viridis::viridis()`.
#' - `viridis::viridis_pal()`.
#' - `viridis::scale_color_viridis_c()`.
#' - `viridis::scale_color_viridis_d()`.
#' - `viridis::scale_fill_viridis_c()`.
#' - `viridis::scale_fill_viridis_d()`.
#'
#' Useful color palette URLs:
#'
#' - http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#' - https://matplotlib.org/tutorials/colors/colormaps.html
#' - http://colorbrewer2.org/
#' - http://colorspace.r-forge.r-project.org/
#' - http://hclwizard.org/
#' - https://developer.apple.com/design/human-interface-guidelines/
#'
#' Additional color palette functions that may be relevant:
#'
#' - `grDevices::hsv()`, `grDevices::rgb()`, `grDevices::col2rgb()`.
#' - `gplots::col2hex()`.
#' - `ggplot2::continuous_scale()`, `ggplot2::discrete_scale()`.
#' - `scales::gradient_n_pal()`.
#' - `Seurat::CustomPalette()`, `Seurat::PurpleAndYellow()`.
#' - `colorRamps::matlab.like()`, `colorRamps::matlab.like2()`.
#' - `RColorBrewer::display.brewer.all()`.
#' - `viridisLite::viridis.map`.
#'
#' @examples
#' library(ggplot2)
#' theme_set(acid_theme_light())
#'
#' ## Color, continuous.
#' cc <- ggplot(mpg, aes(cty, hwy)) + geom_jitter(aes(color = hwy))
#' cc + scale_color_synesthesia_c()
#'
#' ## Color, discrete.
#' cd <- ggplot(mpg, aes(fl)) + geom_bar(aes(color = fl), fill = NA)
#' cd + scale_color_synesthesia_d()
#'
#' ## Fill, continuous.
#' fc <- ggplot(diamonds, aes(carat, price)) + geom_hex()
#' fc + scale_fill_synesthesia_c()
#'
#' ## Fill, discrete.
#' fd <- ggplot(mpg, aes(fl)) + geom_bar(aes(fill = fl))
#' fd + scale_fill_synesthesia_d()
#'
#' ## Base color.
#' par(mar = rep(2.5, times = 4L))
#' image(
#'     matrix(data = seq_len(100L), nrow = 10, ncol = 10),
#'     col = synesthesia()
#' )
NULL



## Previously:
## nolint start
## > colors = c(
## >     "darkorchid3",
## >     "purple1",
## >     "dodgerblue",
## >     "green2",
## >     "orange1",
## >     "darkorange2"
## > )
## nolint end



#' @rdname synesthesia
#' @export
synesthesia <- function(n, palette) {
    palette <- match.arg(palette)
    colors <- get(palette)
    colorNames <- c("purple", "blue", "green", "orange")
    assert(isSubset(colorNames, names(colors)))
    colors <- colors[colorNames]
    gradient(colors = colors, n = n)
}

formals(synesthesia)[c("n", "palette")] <- list(.n, .palette)



#' @rdname synesthesia
#' @export
synesthesia_pal <-  # nolint
    function(palette) {
        palette <- match.arg(palette)
        function(n) {
            synesthesia(n, palette = palette)
        }
    }

formals(synesthesia_pal)[["palette"]] <- .palette



#' @rdname synesthesia
#' @export
scale_colour_synesthesia_c <-  # nolint
    function(
        ...,
        na.value = "grey50",  # nolint
        guide = "colourbar",
        palette
    ) {
        palette <- match.arg(palette)
        colours <- synesthesia(palette = palette)
        palette <- gradient_n_pal(colours = colours)
        continuous_scale(
            aesthetics = "colour",
            scale_name = "synesthesia",
            palette = palette,
            na.value = na.value,
            guide = guide,
            ...
        )
    }

formals(scale_colour_synesthesia_c)[["palette"]] <- .palette



#' @rdname synesthesia
#' @export
scale_color_synesthesia_c <-  # nolint
    scale_colour_synesthesia_c



#' @rdname synesthesia
#' @export
scale_colour_synesthesia_d <-  # nolint
    function(..., palette) {
        palette <- match.arg(palette)
        palette <- synesthesia_pal(palette = palette)
        discrete_scale(
            aesthetics = "colour",
            scale_name = "synesthesia",
            palette = palette,
            ...
        )
    }

formals(scale_colour_synesthesia_d)[["palette"]] <- .palette



#' @rdname synesthesia
#' @export
scale_color_synesthesia_d <-  # nolint
    scale_colour_synesthesia_d



#' @rdname synesthesia
#' @export
scale_fill_synesthesia_c <-  # nolint
    function(
        ...,
        na.value = "grey50",  # nolint
        guide = "colourbar",
        palette
    ) {
        palette <- match.arg(palette)
        colours <- synesthesia(palette = palette)
        palette <- gradient_n_pal(colours = colours)
        continuous_scale(
            aesthetics = "fill",
            scale_name = "synesthesia",
            palette = palette,
            na.value = na.value,
            guide = guide,
            ...
        )
    }

formals(scale_fill_synesthesia_c)[["palette"]] <- .palette



#' @rdname synesthesia
#' @export
scale_fill_synesthesia_d <-  # nolint
    function(..., palette) {
        palette <- match.arg(palette)
        palette <- synesthesia_pal(palette = palette)
        discrete_scale(
            aesthetics = "fill",
            scale_name = "synesthesia",
            palette = palette,
            ...
        )
    }

formals(scale_fill_synesthesia_d)[["palette"]] <- .palette
