#' Synesthesia color palette
#'
#' @rdname synesthesia
#' @name synesthesia
#'
#' @seealso
#' Adapted from viridis color palette:
#'
#' - `viridis::viridis()`.
#' - `viridis::viridis_pal()`.
#' - `viridis::scale_colour_viridis_c()`.
#' - `viridis::scale_colour_viridis_d()`.
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
#'
#' ## ggplot continuous colour
#'
#' ## ggplot discrete colour
#'
#' ## ggplot continuous fill
#' p <- ggplot(diamonds, aes(carat, price)) + geom_hex()
#' p + scale_fill_viridis_c()
#' p + scale_fill_synesthesia_c()
#'
#' ## ggplot discrete fill
#'
#' ## base color
#' image(matrix(1:400, 20), col = synesthesia())
NULL



#' @rdname synesthesia
#' @export
synesthesia <- function(n = 256L) {
    assert(isInt(n), isPositive(n))
    colors <- vapply(
        X = c(
            "darkorchid3", "purple1",
            "dodgerblue",
            "green2",
            "orange1", "darkorange2"
        ),
        FUN = col2hex,
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    colorRampPalette(colors = colors, space = "rgb")(n)
}



#' @rdname synesthesia
#' @export
synesthesia_pal <-  # nolint
    function() {
        function(n) {
            synesthesia(n)
        }
    }



#' @rdname synesthesia
#' @export
scale_colour_synesthesia_c <-  # nolint
    function(
        ...,
        na.value = "grey50",
        guide = "colourbar"
    ) {
        continuous_scale(
            aesthetics = "colour",
            scale_name = "synesthesia",
            palette = gradient_n_pal(colours = synesthesia()),
            na.value = na.value,
            guide = guide,
            ...
        )
    }



#' @rdname synesthesia
#' @export
scale_color_synesthesia_c <-  # nolint
    scale_colour_synesthesia_c



#' @rdname synesthesia
#' @export
scale_colour_synesthesia_d <-  # nolint
    function(...) {
        discrete_scale(
            aesthetics = "colour",
            scale_name = "synesthesia",
            palette = synesthesia_pal(),
            ...
        )
    }



#' @rdname synesthesia
#' @export
scale_color_synesthesia_d <-  # nolint
    scale_colour_synesthesia_d



#' @rdname synesthesia
#' @export
scale_fill_synesthesia_c <-  # nolint
    function(
        ...,
        na.value = "grey50",
        guide = "colourbar"
    ) {
        continuous_scale(
            aesthetics = "fill",
            scale_name = "synesthesia",
            palette = gradient_n_pal(colours = synesthesia()),
            na.value = na.value,
            guide = guide,
            ...
        )
    }



#' @rdname synesthesia
#' @export
scale_fill_synesthesia_d <-  # nolint
    function(...) {
        discrete_scale(
            aesthetics = "fill",
            scale_name = "synesthesia",
            palette = synesthesia_pal(),
            ...
        )
    }
