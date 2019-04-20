# nocov start
# nolint start



#' @name defunct
#' @inherit basejump::defunct
#' @keywords internal
NULL

#' @name deprecated
#' @inherit basejump::deprecated
#' @keywords internal
NULL



# v0.2.0 =======================================================================
#' @rdname deprecated
#' @export
theme_midnight <- function(...) {
    .Deprecated("acid_theme_dark")
    acid_theme_dark(...)
}

#' @rdname deprecated
#' @export
theme_paperwhite <- function(...) {
    .Deprecated("acid_theme_light")
    acid_theme_light(...)
}


# nocov end
# nolint end
