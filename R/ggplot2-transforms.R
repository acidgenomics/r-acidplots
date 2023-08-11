## FIXME Need a working example here.

#' Remove Y axis padding
#'
#' @export
#' @note Updated 2021-06-29.
#'
#' @param expand `numeric`.
#' Range expansion.
#' Refer to `ggplot2::expansion` for details.
#'
#' @param ... Other arguments passed to `scale_y_continuous()`.
#'
#' @return `ggproto`.
#'
#' @examples
#' acid_scale_y_continuous_nopad()
acid_scale_y_continuous_nopad <- # nolint
    function(...,
             expand = ggplot2::expansion(mult = c(0L, 0.025))) {
        scale_y_continuous(..., expand = expand)
    }
