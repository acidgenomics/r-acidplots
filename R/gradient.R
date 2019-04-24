#' Hex color gradient generator
#'
#' @param colors `character`.
#'   Color names or hexadecimal values used to define the gradient.
#' @param n `integer(1)`.
#'   The number of colors (>= 1) to be in the palette.
#'
#' @export
gradient <- function(
    colors = c(
        low = "darkorchid3",
        mid = "gray50",
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
