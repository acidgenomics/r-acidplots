.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



#' Internal formals list, specific to package
#'
#' @note Updated 2021-08-11.
#' @noRd
.formalsList <- list(
    "geom" = c("histogram", "ecdf", "violin", "ridgeline", "boxplot"),
    "n" = 256L,
    "palette" = c(
        "lightPalette",
        "darkPalette",
        "draculaPalette",
        "macOSLightPalette",
        "macOSDarkPalette",
        "iOSLightPalette",
        "iOSDarkPalette"
    )
)



#' AcidPlots test data URL
#'
#' @keywords internal
#' @export
#'
#' @examples
#' AcidPlotsTestsURL
AcidPlotsTestsURL <-  # nolint
    paste0(
        "https://r.acidgenomics.com/testdata/acidplots/",
        "v", .pkgVersion$major, ".", .pkgVersion$minor  # nolint
    )



## Color palettes ==============================================================

## FIXME Add nord palette here.
## FIXME Add gruvbox palette here.

## nolint start

#' Color palettes
#'
#' Color palette hexadecimal character vectors.
#'
#' @name palettes
#' @note Updated 2021-08-11.
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
NULL

## nolint end



#' Prepare exported palette global variable
#'
#' @note Updated 2021-08-11.
#' @noRd
.preparePalette <- function(name) {
    .palettes[[name]][sort(names(.palettes[[name]]))]
}



#' Improve the `rgb()` default to use 0:255, as expected
#'
#' @note Updated 2019-09-13.
#' @noRd
.rgb <- function(...) {
    rgb(..., maxColorValue = 255L)
}



## Updated 2021-08-11.
.palettes <- list(
    "dracula" = c(
        "background" = .rgb(40L, 42L, 54L),
        "foreground" = .rgb(248L, 248L, 242L),
        "border" = .rgb(54L, 57L, 72L),
        "blue" = .rgb(0L, 122L, 255L),
        "green" = .rgb(52L, 199L, 89L),
        "orange" = .rgb(255L, 149L, 0L),
        "pink" = .rgb(255L, 45L, 85L),
        "purple" = .rgb(175L, 82L, 222L),
        "red" = .rgb(255L, 59L, 48L),
        "teal" = .rgb(90L, 200L, 250L),
        "yellow" = .rgb(255L, 204L, 0L)
    ),
    "iOSDark" = c(
        "background" = .rgb(23L, 23L, 23L),
        "foreground" = .rgb(153L, 153L, 153L),
        "border" = .rgb(152L, 152L, 157L),
        "blue" = .rgb(10L, 132L, 255L),
        "gray" = .rgb(152L, 152L, 157L),
        "green" = .rgb(48L, 209L, 88L),
        "orange" = .rgb(255L, 159L, 10L),
        "pink" = .rgb(255L, 55L, 95L),
        "purple" = .rgb(191L, 90L, 242L),
        "red" = .rgb(255L, 69L, 58L),
        "teal" = .rgb(100L, 210L, 255L),
        "yellow" = .rgb(255L, 214L, 10L)
    ),
    "iOSLight" = c(
        "background" = .rgb(255L, 255L, 255L),
        "foreground" = .rgb(0L, 0L, 0L),
        "border" = .rgb(142L, 142L, 147L),
        "blue" = .rgb(0L, 122L, 255L),
        "gray" = .rgb(142L, 142L, 147L),
        "green" = .rgb(52L, 199L, 89L),
        "orange" = .rgb(255L, 149L, 0L),
        "pink" = .rgb(255L, 45L, 85L),
        "purple" = .rgb(175L, 82L, 222L),
        "red" = .rgb(255L, 59L, 48L),
        "teal" = .rgb(90L, 200L, 250L),
        "yellow" = .rgb(255L, 204L, 0L)
    )
)

.palettes[["macOSDark"]] <- .palettes[["iOSDark"]]
.palettes[["macOSDark"]][["green"]] <- .rgb(50L, 215L, 75L)

.palettes[["macOSLight"]] <- .palettes[["iOSLight"]]
.palettes[["macOSLight"]][["green"]] <- .rgb(40L, 205L, 65L)

.palettes[["dracula"]][["gray"]] <- .palettes[["macOSDark"]][["gray"]]

.palettes[["light"]] <- .palettes[["macOSLight"]]

.palettes[["dark"]] <- .palettes[["macOSDark"]]
.palettes[["dark"]][["background"]] <- .rgb(0L, 0L, 0L)
.palettes[["dark"]][["foreground"]] <- .rgb(255L, 255L, 255L)
.palettes[["dark"]][["gray"]] <- .rgb(26L, 26L, 26L)  ## gray10



#' @rdname palettes
#' @export
lightPalette <- .preparePalette("light")

#' @rdname palettes
#' @export
darkPalette <- .preparePalette("dark")

#' @rdname palettes
#' @export
draculaPalette <- .preparePalette("dracula")

#' @rdname palettes
#' @export
iOSLightPalette <- .preparePalette("iOSLight")

#' @rdname palettes
#' @export
iOSDarkPalette <- .preparePalette("iOSDark")

#' @rdname palettes
#' @export
macOSLightPalette <- .preparePalette("macOSLight")

#' @rdname palettes
#' @export
macOSDarkPalette <- .preparePalette("macOSDark")
