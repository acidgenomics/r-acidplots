#' @name plotWaterfall
#' @inherit acidgenerics::plotWaterfall
#' @note Updated 2020-06-25.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
NULL



#' @rdname plotWaterfall
#' @name plotWaterfall
#' @importFrom acidgenerics plotWaterfall
#' @usage plotWaterfall(object, ...)
#' @export
NULL



## Updated 2020-06-25.
`plotWaterfall,data.frame` <- function(
    object,
    xCol, yCol,
    interestingGroups,
    labels = NULL
) {
    validObject(object)
    object <- as.data.frame(object)
    assert(isSubset(interestingGroups, colnames(object)))
    labels <- matchLabels(
        labels = labels,
        choices = eval(formals()[["labels"]])
    )
    facet <- do.call(
        what = paste,
        args = c(
            object[, interestingGroups, drop = FALSE],
            sep = ":"
        )
    )
    data <- data.frame(
        x = reorder(object[[xCol]], -object[[yCol]]),
        y = object[[yCol]],
        facet = facet,
        label = sprintf("%.2f", round(object[[yCol]], 2L))
    )
    p <- ggplot(
        data = data,
        mapping = aes(
            x = !!sym("x"),
            y = !!sym("y"),
            fill = !!sym("facet"),
            label = !!sym("label")
        )
    ) +
        geom_bar(stat = "identity") +
        geom_text(
            angle = 90L,
            hjust = 0L,
            nudge_y = 0.1
        ) +
        scale_fill_synesthesia_d() +
        facet_grid(
            cols = vars(!!sym("facet")),
            scales = "free_x",
            space = "free_x"
        ) +
        theme(
            strip.text.x = element_text(
                angle = 90L,
                hjust = 0L,
                margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
            ),
            axis.text.x = element_text(
                angle = 90,
                hjust = 1L
            )
        )
    ## Labels.
    if (is.list(labels)) {
        if (is.null(labels[["x"]])) labels[["x"]] <- xCol
        if (is.null(labels[["y"]])) labels[["y"]] <- yCol
        if (is.null(labels[["fill"]])) {
            labels[["fill"]] <- paste(interestingGroups, sep = "\n")
        }
        p <- p + do.call(what = labs, args = labels)
    }
    p
}



#' @rdname plotWaterfall
#' @export
setMethod(
    f = "plotWaterfall",
    signature = signature("data.frame"),
    definition = `plotWaterfall,data.frame`
)
