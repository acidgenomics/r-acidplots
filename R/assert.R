## FIXME Consider exporting these in goalie, and removing unnecessary internal
## asserts defined here.
## FIXME Rework and improve consistency of these with AcidSingleCell package.




## Updated 2019-07-31.
.hasMultipleSamples <- function(object) {
    length(sampleNames(object)) > 1L
}



## Consider moving this to goalie package.
## Updated 2019-07-31.
.isBPPARAM <- function(object) {
    all(
        identical(
            attributes(class(object))[["package"]],
            "BiocParallel"
        ),
        grepl("Param$", class(object))
    )
}



#' Should the plot labels be rendered in dark mode?
#'
#' @export
#' @note Updated 2019-09-13.
#'
#' @return `logical(1)`.
#'
#' @examples
#' isDark()
isDark <- function() {
    isTRUE(getOption(x = "acid.dark"))
}
