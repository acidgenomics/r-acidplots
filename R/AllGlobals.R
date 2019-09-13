globalVariables(".")

.version <- packageVersion("acidplots")

#' acidplots test data URL
#' @keywords internal
#' @export
#' @examples
#' acidplotsTestsURL
acidplotsTestsURL <- paste0(
    "http://tests.acidgenomics.com/acidplots/",
    "v", .version$major, ".", .version$minor  # nolint
)



## This is also defined in acidplots.
geom <- c("histogram", "ecdf", "violin", "ridgeline", "boxplot")



## Improve the `rgb()` default to use 0:255, as expected.
## Updated 2019-09-13.
.rgb <- function(...) {
    rgb(..., maxColorValue = 255L)
}



## Color palettes ==============================================================

## nolint start

#' Color palettes
#'
#' Color palette hexadecimal character vectors.
#'
#' @name palettes
#' @note Updated 2019-09-13.
#'
#' @return `character`.
#'
#' @seealso
#' - [iOS palette](https://developer.apple.com/design/human-interface-guidelines/ios/visual-design/color/)
#' - [macOS palette](https://developer.apple.com/design/human-interface-guidelines/macos/visual-design/color/)
#' - [Dracula palette](https://github.com/dracula/dracula-theme)
#'
#' @examples
#' lightPalette
#' darkPalette
NULL

## nolint end



.n <- 256L
.scheme <- c("light", "dark")



#' @rdname palettes
#' @export
iOSLightPalette <- c(
    background = .rgb(255L, 255L, 255L),
    foreground = .rgb(0L, 0L, 0L),
    blue = .rgb(0L, 122L, 255L),
    gray = .rgb(142L, 142L, 147L),
    green = .rgb(52L, 199L, 89L),
    orange = .rgb(255L, 149L, 0L),
    pink = .rgb(255L, 45L, 85L),
    purple = .rgb(175L, 82L, 222L),
    red = .rgb(255L, 59L, 48L),
    teal = .rgb(90L, 200L, 250L),
    yellow = .rgb(255L, 204L, 0L)
)



#' @rdname palettes
#' @export
iOSDarkPalette <- c(
    background = .rgb(23L, 23L, 23L),
    foreground = .rgb(153L, 153L, 153L),
    blue = .rgb(10L, 132L, 255L),
    gray = .rgb(152L, 152L, 157L),
    green = .rgb(48L, 209L, 88L),
    orange = .rgb(255L, 159L, 10L),
    pink = .rgb(255L, 55L, 95L),
    purple = .rgb(191L, 90L, 242L),
    red = .rgb(255L, 69L, 58L),
    teal = .rgb(100L, 210L, 255L),
    yellow = .rgb(255L, 214L, 10L)
)



#' @rdname palettes
#' @export
macOSLightPalette <- iOSLightPalette
macOSLightPalette[["green"]] <- .rgb(40L, 205L, 65L)



#' @rdname palettes
#' @export
macOSDarkPalette <- iOSDarkPalette
macOSDarkPalette[["green"]] <- .rgb(50L, 215L, 75L)



#' @rdname palettes
#' @export
draculaPalette <- c(
    background = .rgb(40L, 42L, 54L),
    foreground = .rgb(248L, 248L, 242L),
    blue = .rgb(0L, 122L, 255L),
    green = .rgb(52L, 199L, 89L),
    orange = .rgb(255L, 149L, 0L),
    pink = .rgb(255L, 45L, 85L),
    purple = .rgb(175L, 82L, 222L),
    red = .rgb(255L, 59L, 48L),
    teal = .rgb(90L, 200L, 250L),
    yellow = .rgb(255L, 204L, 0L)
)
draculaPalette[["gray"]] <- macOSDarkPalette[["gray"]]



#' @rdname palettes
#' @export
darkPalette <- macOSDarkPalette
darkPalette[c("background", "foreground", "gray")] <-
    list(
        .rgb(0L, 0L, 0L),
        .rgb(255L, 255L, 255L),
        .rgb(26L, 26L, 26L)  ## gray10
    )



#' @rdname palettes
#' @export
lightPalette <- macOSLightPalette
