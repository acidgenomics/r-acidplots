#' @name plotCleveland
#' @inherit AcidGenerics::plotCleveland
#' @note Updated 2024-04-17.
#'
#' @param xCol,yCol `character(1)` or `integer(1)`.
#' X and Y column name or position.
#'
#' @examples
#' ## FIXME Need to add working example.
NULL



## Updated 2024-04-05.
`plotCleveland,data.frame` <- # nolint
    function(object,
             xCol,
             yCol,
             labels = list(
                 "title" = NULL,
                 "subtitle" = NULL,
                 "x" = NULL,
                 "y" = NULL
             ),
             color = "black") {
        assert(
            validObject(object),
            hasCols(object),
            hasRows(object),
            isString(xCol) || isInt(xCol),
            isString(yCol) || isInt(yCol),
            isString(color)
        )
        df <- as.data.frame(object)
        df <- df[, c(xCol, yCol), drop = FALSE]
        if (is.null(labels[["x"]])) {
            labels[["x"]] <- colnames(df)[[1L]]
        }
        if (is.null(labels[["y"]])) {
            labels[["y"]] <- colnames(df)[[2L]]
        }
        labs <- do.call(what = labs, args = labels)
        assert(is(labs, "labels"))
        colnames(df) <- c("x", "y")
        df <- df[complete.cases(df), , drop = FALSE]
        assert(
            hasRows(df),
            hasNoDuplicates(df[["x"]])
        )
        idx <- order(df[["y"]])
        df <- df[idx, , drop = FALSE]
        df[["x"]] <- factor(x = df[["x"]], levels = df[["x"]])
        p <- ggplot(
            data = df,
            mapping = aes(
                x = .data[["x"]],
                y = .data[["y"]]
            )
        ) +
            geom_point(color = color)
        if (nrow(df) > 20L) {
            p <- p + scale_x_discrete(labels = NULL, breaks = NULL)
        }
        p <- p + labs
        p
    }



## Updated 2023-04-27.
`plotCleveland,DFrame` <- # nolint
    `plotCleveland,data.frame`



#' @rdname plotCleveland
#' @export
setMethod(
    f = "plotCleveland",
    signature = signature(object = "DFrame"),
    definition = `plotCleveland,DFrame`
)

#' @rdname plotCleveland
#' @export
setMethod(
    f = "plotCleveland",
    signature = signature(object = "data.frame"),
    definition = `plotCleveland,data.frame`
)
