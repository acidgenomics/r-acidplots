## FIXME RETHINK THE METHOD SUPPORT HERE.



## nolint start

#' @name plotWaterfall
#' @inherit AcidGenerics::plotWaterfall
#' @note Updated 2021-02-09.
#'
#' @inheritParams AcidRoxygen::params
#' @param sampleCol `character(1)`.
#'   Column name of discrete samples to plot on X axis.
#' @param valueCol `character(1)`.
#'   Column name of continues values to plot on Y axis.
#' @param ... Additional arguments.
#'
#' @seealso
#' - [Ordering bars from lowest to highest value in each facet](https://stackoverflow.com/questions/43176546)
#' - [Ordering bars within facets using tidytext](https://www.r-bloggers.com/2019/12/how-to-reorder-arrange-bars-with-in-each-facet-of-ggplot/)
#'
#' @examples
#' ## data.frame ====
#' object <- data.frame(
#'     "cellId" = paste("cell", seq_len(12L), sep = "_"),
#'     "ic50" = seq(
#'         from = 0.1,
#'         to = 10L,
#'         length.out = 12L
#'     ),
#'     "tumorType" = rep(
#'         x = c("breast", "bladder"),
#'         times = 6L
#'     ),
#'     "tumorSubtype" = rep(
#'         x = c("benign", "malignant"),
#'         each = 6L
#'     )
#' )
#' plotWaterfall(
#'     object = object,
#'     sampleCol = "cellId",
#'     valueCol = "ic50",
#'     interestingGroups = c("tumorType", "tumorSubtype"),
#'     trans = "log10"
#' )
#' plotWaterfall(
#'     object = object,
#'     sampleCol = "cellId",
#'     valueCol = "ic50",
#'     trans = "identity"
#' )
NULL

## nolint end



## Updated 2021-02-09.
`plotWaterfall,data.frame` <-  # nolint
    function(
        object,
        sampleCol,
        valueCol,
        interestingGroups = NULL,
        trans = c("log10", "log2", "identity"),
        fill = purpleOrange(1L)
    ) {
        validObject(object)
        object <- as.data.frame(object)
        assert(
            isString(sampleCol),
            isString(valueCol),
            isSubset(c(sampleCol, valueCol), colnames(object)),
            isCharacter(interestingGroups, nullOK = TRUE),
            isString(fill)
        )
        trans <- match.arg(trans)
        isLog <- !identical(trans, "identity")
        data <- data.frame(
            "x" = object[[sampleCol]],
            "y" = object[[valueCol]]
        )
        if (isTRUE(isLog)) {
            logFun <- get(trans, inherits = TRUE)
            assert(is.function(logFun))
            data[["y"]] <- logFun(data[["y"]])
        }
        if (!is.null(interestingGroups)) {
            assert(isSubset(interestingGroups, colnames(object)))
            data[["facet"]] <- do.call(
                what = paste,
                args = c(object[, interestingGroups, drop = FALSE], sep = ":")
            )
            list <- split(x = data, f = as.factor(data[["facet"]]))
        } else {
            list <- list("unknown" = data)
        }
        assert(hasLength(list))
        plotlist <- mapply(
            title = names(list),
            data = list,
            FUN = function(title, data) {
                data[["x"]] <- reorder(data[["x"]], data[["y"]])
                p <- ggplot(
                    data = data,
                    mapping = aes(x = !!sym("x"), y = !!sym("y"))
                ) +
                    geom_bar(
                        color = NA,
                        fill = fill,
                        show.legend = FALSE,
                        stat = "identity",
                        width = 1L
                    )
                if (isTRUE(isLog)) {
                    p <- p + geom_hline(
                        color = "black",
                        linetype = "solid",
                        size = 0.5,
                        yintercept = 0L
                    )
                }
                ## Labels.
                labels <- list(
                    "x" = NULL,
                    "y" = valueCol
                )
                if (!identical(title, "unknown")) {
                    labels[["title"]] <- title
                }
                if (isTRUE(isLog)) {
                    labels[["y"]] <- paste(trans, labels[["y"]])
                }
                p <- p + do.call(what = labs, args = labels)
                p <- p + theme(
                    legend.position = "none",
                    strip.text.x = element_text(
                        angle = 90L,
                        hjust = 0L,
                        margin = margin(0.2, 0.2, 0.2, 0.2, "cm")
                    )
                )
                ## Dynamically hide x-axis labels if there are a lot of samples.
                if (length(unique(data[["x"]])) <= 100L) {
                    p <- p + theme(
                        axis.text.x = element_text(angle = 90L, hjust = 1L)
                    )
                } else {
                    p <- p + theme(
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.x = element_blank()
                    )
                }
            },
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        )
        plot_grid(plotlist = plotlist)
    }



#' @rdname plotWaterfall
#' @export
setMethod(
    f = "plotWaterfall",
    signature = signature("data.frame"),
    definition = `plotWaterfall,data.frame`
)



## Updated 2021-02-09.
`plotWaterfall,DataFrame` <-  # nolint
    `plotWaterfall,data.frame`



#' @rdname plotWaterfall
#' @export
setMethod(
    f = "plotWaterfall",
    signature = signature("DataFrame"),
    definition = `plotWaterfall,DataFrame`
)
