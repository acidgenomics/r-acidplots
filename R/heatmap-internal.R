## Return hierarchical clustering rows and columns for heatmap return.
## Return `FALSE` (not `NULL`) to skip.
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
        message(paste0(
            "Performing hierarchical clustering.\n",
            "Using stats::hclust(method = ", deparse(method), ")."
        ))
    }

    if (isTRUE(rows)) {
        message("Arranging rows using hclust().")
        out[["rows"]] <- tryCatch(
            expr = hclust(
                d = dist(object),
                method = method
            ),
            error = function(e) {
                warning("hclust() row calculation failed. Skipping.")
                FALSE
            }
        )
    }

    if (isTRUE(cols)) {
        message("Arranging columns using hclust().")
        out[["cols"]] <- tryCatch(
            expr = hclust(
                ## Note the use of `t()` here.
                d = dist(t(object)),
                method = method
            ),
            error = function(e) {
                warning("hclust() column calculation failed. Skipping.")
                FALSE
            }
        )
    }

    out
}



## Apply z-scaling to matrix.
## When scaling by row, drop features without sufficient variance and inform.
## Columns require sufficient variation and will error intentionally otherwise.
## Modified version of `pheatmap:::scale_mat()`.
.scaleMatrix <- function(object, scale = c("none", "row", "column")) {
    assert(is.matrix(object), is.numeric(object))
    scale <- match.arg(scale)

    if (scale != "none") {
        message(paste0("Scaling matrix per ", scale, " (z-score)."))
    }

    ## Inform the user if NA values are present.
    ## Note that we're including `na.rm` in `rowVars()` and `colVars()` calls
    ## below to handle this edge case.
    if (any(is.na(object))) {
        message("NA values detected in matrix.")
    }

    ## Assert checks to look for sufficient variance when the user is attempting
    ## to apply scaling (z-score). Currently we're keeping this very strict and
    ## only looking to see if there is non-zero variance.
    varThreshold <- 0L

    ## Here we're dropping rows (features) without sufficient variation
    ## automatically. The function errors out intentionally if columns (samples)
    ## don't have sufficient variation.
    if (scale == "row") {
        pass <- rowVars(object, na.rm = TRUE) > varThreshold
        if (!all(pass)) {
            fail <- !pass
            n <- sum(fail, na.rm = TRUE)
            message(paste(
                sprintf(ngettext(
                    n = n,
                    msg1 = "%s row doesn't",
                    msg2 = "%s rows don't"
                ), n),
                "have enough variance:",
                toString(rownames(object)[which(fail)], width = 200L)
            ))
            object <- object[pass, , drop = FALSE]
        }
    } else if (scale == "column") {
        pass <- colVars(object, na.rm = TRUE) > varThreshold
        if (!all(pass)) {
            fail <- !pass
            n <- sum(fail, na.rm = TRUE)
            stop(paste(
                sprintf(ngettext(
                    n = n,
                    msg1 = "%s column doesn't",
                    msg2 = "%s columns don't"
                ), n),
                "have enough variance:",
                toString(colnames(object)[which(fail)], width = 200L)
            ))
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



.scaleCols <- function(object) {
    t(.scaleRows(t(object)))
}



.scaleRows <- function(object) {
    assert(is.matrix(object), is.numeric(object))
    mean <- apply(object, MARGIN = 1L, FUN = mean, na.rm = TRUE)
    sd <- apply(object, MARGIN = 1L, FUN = sd, na.rm = TRUE)
    out <- (object - mean) / sd
    out
}
