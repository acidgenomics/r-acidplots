#' Hex color gradient generator
#'
#' @note Updated 2019-09-13.
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



## FIXME Can we soften this, using iOS palette?
#' @rdname gradient
#' @export
purpleOrange <- function(n) {
    palette <- lightPalette
    gradient(
        colors = c(
            low = palette[["purple"]],
            mid = palette[["gray"]],
            high = palette[["orange"]]
        ),
        n = n
    )
}

formals(purpleOrange)[["n"]] <- .formalsList[["n"]]



## FIXME Can we soften this, using iOS palette?
#' @rdname gradient
#' @export
blueYellow <- function(n) {
    palette <- lightPalette
    gradient(
        colors = c(
            low = palette[["blue"]],
            mid = "black",
            high = palette[["yellow"]]
        ),
        n = n
    )
}

formals(blueYellow)[["n"]] <- .formalsList[["n"]]
