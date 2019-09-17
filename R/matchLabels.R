#' Match ggplot2 labels
#'
#' Allow user to define ggplot labels, and populate missing values from the
#' defaults specified in `labels` formal argument.
#'
#' @export
#' @note Attempting to follow a similar naming convention to
#'   [`match.arg()`][base::match.arg] here, using `choices` argument.
#' @note Updated 2019-09-16.
#'
#' @param labels `list`.
#'   User-defined plot labels.
#' @param choices `list`.
#'   Default plot labels, defined in `labels` [`formals()`][base::formals].
#'
#' @seealso [ggplot2::labs()].
#'
#' @examples
#' matchLabels(
#'     labels = list(title = "XXX"),
#'     choices = list(
#'         title = "AAA",
#'         subtitle = "BBB"
#'     )
#' )
matchLabels <- function(labels, choices) {
    assert(
        is.list(labels) || is.null(labels),
        is.list(choices)
    )
    if (is.list(labels)) {
        assert(
            isSubset(names(labels), names(choices)),
            all(bapply(X = labels, FUN = isString, nullOK = TRUE))
        )
    }
    ## Allow the user to pass in a subset of labels, and populate the
    ## rest using the defaults.
    if (!areSetEqual(names(labels), names(choices))) {
        diff <- setdiff(names(choices), names(labels))
        labels <- c(labels, choices[diff])
    }
    labels
}
