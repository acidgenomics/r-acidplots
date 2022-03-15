#' Hex color gradient generator
#'
#' @note Updated 2022-03-07.
#' @export
#'
#' @param colors `character`.
#' Color names or hexadecimal values used to define the gradient.
#'
#' @param palette `character(1)`.
#' Color palette name.
#'
#' @param n `integer(1)`.
#' The number of colors (>= 1) to be in the palette.
#'
#' @return `character`.
#' Hexadecimal colors in RGB space.
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



#' Internal gradient palette handler
#'
#' @note Updated 2021-08-11.
#' @noRd
.gradientPalette <- function(n, palette, colors) {
    palette <- get(
        x = match.arg(palette),
        envir = asNamespace(.pkgName),
        inherits = FALSE
    )
    assert(
        isCharacter(palette),
        allAreHexColors(palette)
    )
    palette[["black"]] <- .rgb(0L, 0L, 0L)
    palette[["white"]] <- .rgb(255L, 255L, 255L)
    assert(
        identical(names(colors), c("low", "mid", "high")),
        isSubset(colors, names(palette))
    )
    gradient(
        colors = c(
            "low" = palette[[colors[["low"]]]],
            "mid" = palette[[colors[["mid"]]]],
            "high" = palette[[colors[["high"]]]]
        ),
        n = n
    )
}

formals(.gradientPalette)[c("n", "palette")] <-
    .formalsList[c("n", "palette")]



#' @rdname gradient
#' @export
blueYellow <- function(n, palette) {
    .gradientPalette(
        n = n,
        palette = match.arg(palette),
        colors = c(
            "low" = "blue",
            "mid" = "black",
            "high" = "yellow"
        )
    )
}

formals(blueYellow)[c("n", "palette")] <-
    formals(.gradientPalette)[c("n", "palette")]



#' @rdname gradient
#' @export
purpleOrange <- function(n, palette) {
    .gradientPalette(
        n = n,
        palette = match.arg(palette),
        colors = c(
            "low" = "purple",
            "mid" = "gray",
            "high" = "orange"
        )
    )
}

formals(purpleOrange)[c("n", "palette")] <-
    formals(.gradientPalette)[c("n", "palette")]
