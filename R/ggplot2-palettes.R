#' Synesthesia color palette
#'
#' @export
#'
#' @seealso
#' - `colorRamps::matlab.like()`, `colorRamps::matlab.like2()`.
#' - `RColorBrewer::display.brewer.all()`.
#' - `grDevices::hsv()`, `grDevices::rgb()`, `grDevices::col2rgb()`.
#' - `gplots::col2hex()`.
#' - `ggplot2::continuous_scale()`, `ggplot2::discrete_scale()`.
#' - `scales::gradient_n_pal()`.
#' - `viridisLite::viridis.map`, `viridis::viridis()`, `viridis::viridis_pal()`.
#' - `Seurat::CustomPalette()`, `Seurat::PurpleAndYellow()`.
#'
#' Additional URLs:
#' - http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#' - https://matplotlib.org/tutorials/colors/colormaps.html
#' - https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
#' - http://colorbrewer2.org/
#' - http://colorspace.r-forge.r-project.org/
#' - http://hclwizard.org/
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
            "dodgerblue",
            "chartreuse",
            "orange"
        ),
        FUN = col2hex,
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    colorRampPalette(colors = colors, space = "rgb")(n)
}



#' @rdname synesthesia
#' @export
synesthesia_pal <- function() {
    function(n) {
        synesthesia(n)
    }
}



#' @importFrom ggplot2 continuous_scale discrete_scale
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 discrete_scale
NULL



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



# h <- ggplot(diamonds, aes(carat, price)) + geom_hex()
# h + scale_fill_gradientn(colours = c("purple", "green", "orange"))
# h + scale_fill_viridis_c()


# .purple_orange_fill <-
#     scale_fill_gradient(low = "orange", high = "purple")


# scale_fill_gradient

# colorRampPalette(brewer.pal(n = 7L, name = "RdYlBu"))(256L)

# viridis::viridis(n = 256L)

# ggplot2::scale_colour_viridis_c()
# ggplot2::scale_colour_viridis_d()
# ggplot2::scale_fill_viridis_c()
# ggplot2::scale_fill_viridis_d()


# purple
# green
# orange
