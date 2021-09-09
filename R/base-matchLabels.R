#' Match ggplot2 labels
#'
#' Allow user to define ggplot labels, and populate missing values from the
#' defaults specified in `labels` formal argument.
#'
#' Attempting to follow a similar naming convention to `match.arg()` here,
#' using `choices` argument
#'
#' @export
#' @note Updated 2021-09-09.
#'
#' @param labels `list`.
#'   User-defined plot labels.
#'   Per element, supports `character(1)`., `logical(1)`
#'   (for automatic labels), or `NULL`.
#' @param choices `list`.
#'   Default plot labels, defined in `labels`.
#'   Refer to `formals()` for details.
#'
#' @seealso
#' - `ggplot2::labs()`.
#'
#' @return `list`.
#' Returns an empty list on `NULL` input.
#'
#' @examples
#' matchLabels(
#'     labels = list(title = "XXX"),
#'     choices = list(
#'         title = "AAA",
#'         subtitle = "BBB"
#'     )
#' )
matchLabels <- function(labels, choices = NULL) {
    ## FIXME Don't allow NULL input of choices.
    if (is.null(labels)) {
        return(list())
    }
    assert(
        is.list(labels),
        all(bapply(
            X = labels,
            FUN = function(x) {
                isString(x) || isFlag(x) || is.null(x)
            }
        )),
        msg = "Invalid 'labels' input."
    )
    if (!is.null(choices)) {
        assert(
            is.list(choices),
            isSubset(names(labels), names(choices))
        )
    }
    ## Allow the user to pass in a subset of labels, and populate the rest
    ## using the defaults.
    if (!areSetEqual(names(labels), names(choices))) {
        diff <- setdiff(names(choices), names(labels))
        labels <- c(labels, choices[diff])
    }
    labels
}
