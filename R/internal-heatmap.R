#' Return hierarchical clustering rows and columns for heatmap return
#' @note Updated 2020-08-25.
#' @return `hclust` object or `FALSE` (not `NULL`) to skip.
#' @noRd
.hclust <- function(
    object,
    method = "ward.D2",
    rows = TRUE,
    cols = TRUE
) {
    assert(
        is.matrix(object),
        is.numeric(object),
        isString(method),
        isFlag(rows),
        isFlag(cols)
    )
    ## Prepare our skeleton return list.
    out <- list(rows = FALSE, cols = FALSE)
    if (isTRUE(rows) || isTRUE(cols)) {
        cli_alert(sprintf(
            fmt = paste0(
                "Performing hierarchical clustering with ",
                "{.fun hclust} method {.arg %s}."
            ),
            deparse(method)
        ))
    }
    if (isTRUE(rows)) {
        out[["rows"]] <- tryCatch(
            expr = hclust(
                d = dist(object),
                method = method
            ),
            error = function(e) {
                ## nocov start
                cli_alert_warning("{.fun hclust} row calculation failed.")
                FALSE
                ## nocov end
            }
        )
    }
    if (isTRUE(cols)) {
        out[["cols"]] <- tryCatch(
            expr = hclust(
                ## Note the use of `t()` here.
                d = dist(t(object)),
                method = method
            ),
            error = function(e) {
                ## nocov start
                cli_alert_warning("{.fun hclust} column calculation failed.")
                FALSE
                ## nocov end
            }
        )
    }
    out
}



#' Apply z-scaling to matrix
#'
#' When scaling by row, drop features without sufficient variance and inform.
#' Columns require sufficient variation and will error intentionally otherwise.
#' Modified version of `pheatmap:::scale_mat()`.
#'
#' @note Updated 2019-07-29.
#' @noRd
.scaleMatrix <- function(object, scale = c("none", "row", "column")) {
    assert(is.matrix(object), is.numeric(object))
    scale <- match.arg(scale)
    ## Inform the user if NA values are present. Note that we're including
    ## `na.rm` in `rowVars()` and `colVars()` calls below to handle this edge
    ## case.
    if (any(is.na(object))) {
        cli_alert_warning("NA values detected in matrix.")  # nocov
    }
    if (!identical(scale, "none")) {
        cli_alert(sprintf("Scaling matrix per %s (z-score).", scale))
    }
    ## Assert checks to look for sufficient variance when the user is attempting
    ## to apply scaling (z-score). Currently we're keeping this very strict and
    ## only looking to see if there is non-zero variance.
    varThreshold <- 0L
    ## Here we're dropping rows (features) without sufficient variation
    ## automatically. The function errors out intentionally if columns (samples)
    ## don't have sufficient variation.
    if (identical(scale, "row")) {
        pass <- rowVars(object, na.rm = TRUE) > varThreshold
        if (!all(pass)) {
            ## nocov start
            fail <- !pass
            n <- sum(fail, na.rm = TRUE)
            cli_alert_info(sprintf(
                fmt = "%d %s have enough variance: %s.",
                n,
                ngettext(
                    n = n,
                    msg1 = "row doesn't",
                    msg2 = "rows don't"
                ),
                toString(rownames(object)[which(fail)], width = 200L)
            ))
            object <- object[pass, , drop = FALSE]
            ## nocov end
        }
    } else if (identical(scale, "column")) {
        pass <- colVars(object, na.rm = TRUE) > varThreshold
        if (!all(pass)) {
            ## nocov start
            fail <- !pass
            n <- sum(fail, na.rm = TRUE)
            stop(sprintf(
                fmt = "%d %s have enough variance: %s.",
                n,
                ngettext(
                    n = n,
                    msg1 = "column doesn't",
                    msg2 = "columns don't"
                ),
                toString(colnames(object)[which(fail)], width = 200L)
            ))
            ## nocov end
        }
    }
    ## Require at least a 2x2 matrix.
    assert(nrow(object) > 1L, ncol(object) > 1L)
    switch(
        EXPR = scale,
        none = object,
        row = .scaleRows(object),
        column = .scaleCols(object)
    )
}



## Updated 2019-07-29.
.scaleCols <- function(object) {
    assert(is.matrix(object), is.numeric(object))
    t(.scaleRows(t(object)))
}



## Updated 2019-07-29.
.scaleRows <- function(object) {
    assert(is.matrix(object), is.numeric(object))
    mean <- apply(object, MARGIN = 1L, FUN = mean, na.rm = TRUE)
    sd <- apply(object, MARGIN = 1L, FUN = sd, na.rm = TRUE)
    out <- (object - mean) / sd
    out
}
