#' Generate ggplot2 labels
#'
#' Consider exporting this function in a future update.
#'
#' @note Updated 2019-09-16.
#' @noRd
.labels <- function(labels, labelsArgs) {
    assert(
        is.list(labels) || is.null(labels),
        is.list(labelsArgs)
    )
    if (is.list(labels)) {
        assert(isSubset(names(labels), names(labelsArgs)))
    }
    ## Allow the user to pass in a subset of labels, and populate the
    ## rest using the defaults.
    if (!areSetEqual(names(labels), names(labelsArgs))) {
        diff <- setdiff(names(labelsArgs), names(labels))
        labels <- c(labels, labelsArgs[diff])
    }
    labels
}
