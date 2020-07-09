#' @name plotWaterfall
#' @inherit acidgenerics::plotWaterfall
#' @note Updated 2020-07-09.
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
#'     trans = "log10",
#'     labels = list(
#'         title = "Effect of compound on cell survival"
#'     )
#' )
#' plotWaterfall(
#'     object = object,
#'     sampleCol = "cell_id",
#'     valueCol = "ic50",
#'     trans = "identity"
#' )
NULL



#' @rdname plotWaterfall
#' @name plotWaterfall
#' @importFrom acidgenerics plotWaterfall
#' @usage plotWaterfall(object, ...)
#' @export
NULL



## Updated 2020-07-09.
`plotWaterfall,data.frame` <-  # nolint
    function(
        object,
        sampleCol,
        valueCol,
        interestingGroups = NULL,
        trans = c("log10", "log2", "identity"),
        label = identical(trans, "identity"),
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
        trans <- match.arg(trans)
        isLog <- !identical(trans, "identity")
        if (isTRUE(isLog) && isTRUE(label)) {
            stop("Bar labeling only supported for `trans = \"identity\"`.")
        }
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
        data <- data.frame(
            x = reorder(object[[sampleCol]], -object[[valueCol]]),
            y = object[[valueCol]],
            label = sprintf("%.1f", round(object[[valueCol]], 2L))
        )
        if (isTRUE(isLog)) {
            logFun <- get(trans, inherits = TRUE)
            assert(is.function(logFun))
            data[["y"]] <- logFun(data[["y"]])
        }
        mapping <- aes(
            x = !!sym("x"),
            y = !!sym("y"),
            label = !!sym("label")
        )
        if (!is.null(interestingGroups)) {
            assert(isSubset(interestingGroups, colnames(object)))
            data[["facet"]] <- do.call(
                what = paste,
                args = c(object[, interestingGroups, drop = FALSE], sep = ":")
            )
            mapping[["fill"]] <- quo(!!sym("facet"))
        }
        p <- ggplot(data = data, mapping = mapping)
        if (isTRUE(label)) {
            p <- p +
                geom_bar(color = "black", stat = "identity", width = 0.9) +
                geom_text(angle = 90L, hjust = 0L, nudge_y = 0.1)
        } else {
            p <- p + geom_bar(color = NA, stat = "identity", width = 1L)
        }
        if (isTRUE(isLog)) {
            p <- p + geom_hline(
                color = "black",
                linetype = "solid",
                size = 0.5,
                yintercept = 0L
            )
        }
        ## Fill.
        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }
        if (!is.null(interestingGroups)) {
            if (length(unique(data[["facet"]])) <= 4L) {
                angle <- 0L
            } else {
                angle <- 90L
            }
            p <- p +
                facet_grid(
                    cols = vars(!!sym("facet")),
                    scales = "free_x",
                    space = "free_x"
                ) +
                theme(
                    legend.position = "none",
                    strip.text.x = element_text(
                        angle = 0L,
                        hjust = 0L,
                        margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
                    )
                )
        }
        ## Dynamically hide x-axis labels if there are a lot of samples.
        if (length(unique(data[["x"]])) <= 50L) {
            p <- p + theme(axis.text.x = element_text(angle = 90L, hjust = 1L))
        } else {
            p <- p + theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank()
            )
        }
        ## Labels.
        if (is.list(labels)) {
            xLab <- sampleCol
            yLab <- valueCol
            if (isTRUE(isLog)) {
                yLab <- paste(trans, yLab)
            }
            if (is.null(labels[["x"]])) labels[["x"]] <- xLab
            if (is.null(labels[["y"]])) labels[["y"]] <- yLab
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
