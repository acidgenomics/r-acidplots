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
#' purpleOrange(n = 3L)
#' blueYellow(n = 3L)
gradient <- function(colors, n) {
    assert(
        isCharacter(colors),
        isInt(n), isPositive(n)
    )
    colorRampPalette(colors = colors, space = "rgb")(n)
}



#' @rdname gradient
#' @export
purpleOrange <- function(n = 256L) {
    gradient(
        colors = c(
            low = "purple",
            mid = "gray80",
            high = "darkorange"
        ),
        n = n
    )
}



#' @rdname gradient
#' @export
blueYellow <- function(n = 256) {
    gradient(
        colors = c(
            low = "dodgerblue",
            mid = "black",
            high = "yellow"
        ),
        n = n
    )
}
