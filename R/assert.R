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
    isTRUE(getOption("acid.dark"))
}
