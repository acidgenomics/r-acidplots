# FIXME Incomplete commit



#' Synesthesia color palette
#'
#' @export
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
#' - `colorRamps::matlab.like()`, `colorRamps::matlab.like2()`.
#' - `viridisLite::viridis.map`.
#' - `Seurat::CustomPalette()`, `Seurat::PurpleAndYellow()`.
#' - `RColorBrewer::display.brewer.all()`.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(diamonds, aes(carat, price)) + geom_hex()
#' p + scale_fill_viridis_c()
#' p + scale_fill_synesthesia_c()
#'
#' image(matrix(1:400, 20), col = synesthesia())
synesthesia <- function(n = 256L) {
    assert(isInt(n), isPositive(n))
    colors <- vapply(
        X = c(
            "purple4",
            "purple1",
            "dodgerblue",
            "green",  # "chartreuse"
            "darkorange",
            "firebrick1",
            "firebrick4"
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
scale_colour_synesthesia_c <- function(
) {
}



#' @rdname synesthesia
#' @export
scale_colour_synesthesia_d <- function(
) {
}



#' @rdname synesthesia
#' @export
scale_fill_synesthesia_c <- function(
    ...,
    na.value = "grey50",
    guide = "colourbar"
) {
    continuous_scale(
        aesthetics = "fill",
        scale_name = "synesthesia_c",
        palette = gradient_n_pal(colours = synesthesia()),
        na.value = na.value,
        guide = guide,
        ...
    )
}



#' @rdname synesthesia
#' @export
scale_fill_synesthesia_d <- function(...) {
    discrete_scale(
        aesthetics = "fill",
        scale_name = "synesthesia_d",
        palette = synesthesia_pal(),
        ...
    )
}
