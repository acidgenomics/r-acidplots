#' Hex color gradient generator
#'
#' @export
#'
#' @param colors `character`.
#'   Color names or hexadecimal values used to define the gradient.
#' @param n `integer(1)`.
#'   The number of colors (>= 1) to be in the palette.
#'
#' @return `character`.
#'   Hexadecimal colors in RGB space.
#'
#' @examples
#' gradient(n = 3L)
gradient <- function(
    colors = c(
        low = "darkorchid3",
        mid = "gray75",
        high = "darkorange2"
    ),
    n = 256L
) {
    assert(
        isCharacter(colors),
        isInt(n), isPositive(n)
    )
    colorRampPalette(colors = colors, space = "rgb")(n)
}
