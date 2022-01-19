#' Match ggplot2 labels
#'
#' Allow user to define ggplot labels, and populate missing values from the
#' defaults specified in `labels` formal argument.
#'
#' @export
#' @note Updated 2022-01-19.
#'
#' @param labels `list`.
#'   User-defined plot labels.
#'   Per element, supports `character(1)`., `logical(1)`
#'   (for automatic labels), or `NULL`.
#' @param argName `character(1)`.
#'   Argument name defined in `formalArgs()` to match against user input.
#'   Defaults to `"labels"`.
#'
#' @seealso
#' - `ggplot2::labs()`.
#'
#' @return `list`.
#' Returns an empty list on `NULL` input.
#'
#' @examples
#' fun <- function(
#'     object,
#'     labels = list(
#'         "title" = NULL,
#'         "x" = "x-axis",
#'         "y" = "y-axis"
#'     )
#' ) {
#'     labels <- matchLabels(labels)
#'     labels
#' }
matchLabels <- function(labels, argName = "labels") {
    if (is.null(labels)) {
        return(list())
    }
    ## This approach is used internally in `match.arg()`.
    formalArgs <- formals(sys.function(sysP <- sys.parent()))
    assert(
        isString(argName),
        isSubset(argName, names(formalArgs))
    )
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
    defaults <- eval(formalArgs[[argName]])
    assert(
        is.list(defaults),
        hasNames(defaults),
        isSubset(names(labels), names(defaults))
    )
    ## Allow the user to pass in a subset of labels, and populate the rest
    ## using the defaults defined in the formal argument.
    if (!areSetEqual(names(labels), names(defaults))) {
        diff <- setdiff(names(defaults), names(labels))
        labels <- append(x = labels, values = defaults[diff])
    }
    labels
}
