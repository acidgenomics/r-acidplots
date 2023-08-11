#' Extract quosure name
#'
#' @note Updated 2023-08-11.
#' @noRd
#'
#' @examples
#' data <- datasets::mtcars
#' xCol <- "mpg"
#'
#' ## Using `!!sym()` approach.
#' p <- ggplot(
#'     data = data,
#'     mapping = aes(x = !!sym(xCol))
#' )
#' mapping <- .detectMapping(p)
#' out <- .extractQuoName(mapping[["x"]])
#' print(out)
#' ## [1] "mpg"
#'
#' ## Using `.data` pronoun approach.
#' p <- ggplot(
#'     data = data,
#'     mapping = aes(x = .data[[xCol]])
#' )
#' mapping <- .detectMapping(p)
#' out <- .extractQuoName(mapping[["x"]])
#' print(out)
#' ## [1] "mpg"
.extractQuoName <- function(quo) {
    assert(is(quo, "quosure"))
    if (quo_is_symbol(quo)) {
        ## e.g. using `!!sym("x")`, extracts `"x"`.
        out <- quo_text(quo)
    } else if (quo_is_symbolic(quo)) {
        ## e.g. using `.data[["x"]]`, returns as unmodified string.
        pronoun <- quo_text(quo)
        pattern <- "^\\.data\\[\\[\"(.+)\"\\]\\]$"
        assert(isMatchingRegex(pattern = pattern, x = pronoun))
        out <- sub(pattern = pattern, replacement = "\\1", x = pronoun)
    } else {
        abort("Unexpected rlang quosure error.")
    }
    assert(isString(out))
    out
}
