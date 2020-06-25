#' @name plotWaterfall
#' @inherit acidgenerics::plotWaterfall
#' @note Updated 2020-06-25.
#'
#' @inheritParams acidroxygen::params
#' @param sampleCol `character(1)`.
#'   Column name of discrete samples to plot on X axis.
#' @param valueCol `character(1)`.
#'   Column name of continues values to plot on Y axis.
#' @param label `logical(1)`.
#'   Include text labels showing the values above each bar.
#' @param ... Additional arguments.
#'
#' @examples
#' ## data.frame ====
#' object <- data.frame(
#'     cell_id = paste("cell", seq_len(12L), sep = "_"),
#'     ic50 = seq(
#'         from = 0.1,
#'         to = 10L,
#'         length.out = 12L
#'     ),
#'     tumor_type = rep(
#'         x = c("breast", "bladder"),
#'         times = 6L
#'     ),
#'     tumor_subtype = rep(
#'         x = c("benign", "malignant"),
#'         each = 6L
#'     )
#' )
#' plotWaterfall(
#'     object = object,
#'     sampleCol = "cell_id",
#'     valueCol = "ic50",
#'     interestingGroups = c("tumor_type", "tumor_subtype"),
#'     labels = list(
#'         title = "Effect of compound on cell survival"
#'     )
#' )
#' plotWaterfall(
#'     object = object,
#'     sampleCol = "cell_id",
#'     valueCol = "ic50",
#'     label = FALSE
#' )
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
    sampleCol,
    valueCol,
    interestingGroups = NULL,
    label = TRUE,
    fill,
    labels = NULL
) {
    validObject(object)
    object <- as.data.frame(object)
    assert(
        isString(sampleCol),
        isString(valueCol),
        isSubset(c(sampleCol, valueCol), colnames(object)),
        isCharacter(interestingGroups, nullOK = TRUE),
        isFlag(label),
        isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE)
    )
    labels <- matchLabels(
        labels = labels,
        choices = eval(formals()[["labels"]])
    )
    data <- data.frame(
        x = reorder(object[[sampleCol]], -object[[valueCol]]),
        y = object[[valueCol]],
        label = sprintf("%.2f", round(object[[valueCol]], 2L))
    )
    mapping <- aes(
        x = !!sym("x"),
        y = !!sym("y"),
        label = !!sym("label")
    )
    if (!is.null(interestingGroups)) {
        assert(isSubset(interestingGroups, colnames(object)))
        data[["facet"]] <- do.call(
            what = paste,
            args = c(
                object[, interestingGroups, drop = FALSE],
                sep = ":"
            )
        )
        mapping[["fill"]] <- quo(!!sym("facet"))
    }
    p <- ggplot(
        data = data,
        mapping = mapping
    ) +
        geom_bar(stat = "identity")
    if (isTRUE(label)) {
        p <- p + geom_text(
            angle = 90L,
            hjust = 0L,
            nudge_y = 0.1
        )
    }
    ## Fill.
    if (is(fill, "ScaleDiscrete")) {
        p <- p + fill
    }
    if (!is.null(interestingGroups)) {
        p <- p + facet_grid(
            cols = vars(!!sym("facet")),
            scales = "free_x",
            space = "free_x"
        )
    }
    p <- p + theme(
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
        if (is.null(labels[["x"]])) labels[["x"]] <- sampleCol
        if (is.null(labels[["y"]])) labels[["y"]] <- valueCol
        if (!is.null(interestingGroups) && is.null(labels[["fill"]])) {
            labels[["fill"]] <- paste(interestingGroups, sep = "\n")
        }
        p <- p + do.call(what = labs, args = labels)
    }
    p
}

f <- formals(`plotWaterfall,data.frame`)
f[["fill"]] <- formalsList[["fill.discrete"]]
formals(`plotWaterfall,data.frame`) <- f



#' @rdname plotWaterfall
#' @export
setMethod(
    f = "plotWaterfall",
    signature = signature("data.frame"),
    definition = `plotWaterfall,data.frame`
)
