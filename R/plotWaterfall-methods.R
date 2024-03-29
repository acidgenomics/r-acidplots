## nolint start

#' @name plotWaterfall
#' @inherit AcidGenerics::plotWaterfall
#' @note Updated 2023-08-10.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param fill `character(1)`.
#' R color name or hex color code (e.g. `"#AF52DE"`).
#'
#' @param sampleCol `character(1)`.
#' Column name of discrete samples to plot on X axis.
#'
#' @param valueCol `character(1)`.
#' Column name of continues values to plot on Y axis.
#'
#' @seealso
#' - [Ordering bars from lowest to highest value in each facet](https://stackoverflow.com/questions/43176546)
#' - [Ordering bars within facets using tidytext](https://www.r-bloggers.com/2019/12/how-to-reorder-arrange-bars-with-in-each-facet-of-ggplot/)
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## data.frame ====
#' object <- data.frame(
#'     "cellId" = AcidGenerics::autopadZeros(
#'         object = paste("cell", seq_len(12L), sep = "_")
#'     ),
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
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotWaterfall(object, trans = "identity")
NULL

## nolint end



## Updated 2021-02-09.
`plotWaterfall,data.frame` <- # nolint
    function(object,
             sampleCol,
             valueCol,
             interestingGroups = NULL,
             trans = c("identity", "log2", "log10"),
             fill = purpleOrange(1L),
             labels = list(
                 "title" = NULL,
                 "subtitle" = NULL
             )) {
        validObject(object)
        object <- as.data.frame(object)
        assert(
            isString(sampleCol),
            isString(valueCol),
            isSubset(c(sampleCol, valueCol), colnames(object)),
            isCharacter(interestingGroups, nullOk = TRUE),
            isString(fill)
        )
        trans <- match.arg(trans)
        isLog <- !identical(trans, "identity")
        labels <- matchLabels(labels)
        data <- data.frame(
            "x" = object[[sampleCol]],
            "y" = object[[valueCol]]
        )
        assert(hasNoDuplicates(data[["x"]]))
        if (isTRUE(isLog)) {
            assert(allArePositive(data[["y"]]))
            logFun <- get(trans, inherits = TRUE)
            assert(is.function(logFun))
            data[["y"]] <- logFun(data[["y"]])
        }
        if (isSubset("interestingGroups", colnames(object))) {
            assert(
                is.null(interestingGroups),
                is.factor(object[["interestingGroups"]])
            )
            data[["facet"]] <- object[["interestingGroups"]]
        } else if (!is.null(interestingGroups)) {
            assert(isSubset(interestingGroups, colnames(object)))
            data[["facet"]] <- as.factor(do.call(
                what = paste,
                args = c(object[, interestingGroups, drop = FALSE], sep = ":")
            ))
        }
        if (is.factor(data[["facet"]])) {
            list <- split(x = data, f = data[["facet"]])
        } else {
            list <- list("unknown" = data)
        }
        assert(hasLength(list))
        plotlist <- Map(
            title = names(list),
            data = list,
            f = function(title, data) {
                data[["x"]] <- reorder(data[["x"]], data[["y"]])
                p <- ggplot(
                    data = data,
                    mapping = aes(x = .data[["x"]], y = .data[["y"]])
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
            }
        )
        ## Using patchwork package to dynamically arrange the plots.
        p <- wrap_plots(plotlist)
        ## Support title and/or subtitle labeling.
        p <- p + plot_annotation(
            "title" = labels[["title"]],
            "subtitle" = labels[["subtitle"]]
        )
        p
    }



## Updated 2021-10-13.
`plotWaterfall,DFrame` <- # nolint
    `plotWaterfall,data.frame`



## Updated 2023-04-27.
`plotWaterfall,SE` <- # nolint
    function(object,
             assay = 1L,
             fun = c("mean", "sum"),
             interestingGroups = NULL,
             ...) {
        assert(validObject(object))
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        sd <- sampleData(object)
        assert(
            identical(rownames(sd), colnames(object)),
            isSubset(
                x = c("interestingGroups", "sampleName"),
                y = colnames(sd)
            )
        )
        assay <- assay(object, i = assay)
        whatPkg <- ifelse(
            test = is(assay, "Matrix"),
            yes = "Matrix",
            no = "base"
        )
        fname <- switch(
            EXPR = match.arg(fun),
            "mean" = "colMeans",
            "sum" = "colSums"
        )
        fun <- get(x = fname, envir = asNamespace(whatPkg), inherits = FALSE)
        assert(is.function(fun))
        data <- DataFrame(
            "sample" = sd[["sampleName"]],
            "interestingGroups" = sd[["interestingGroups"]],
            "value" = fun(assay)
        )
        plotWaterfall(
            object = data,
            sampleCol = "sample",
            valueCol = "value",
            ...
        )
    }



#' @rdname plotWaterfall
#' @export
setMethod(
    f = "plotWaterfall",
    signature = signature(object = "DFrame"),
    definition = `plotWaterfall,DFrame`
)

#' @rdname plotWaterfall
#' @export
setMethod(
    f = "plotWaterfall",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotWaterfall,SE`
)

#' @rdname plotWaterfall
#' @export
setMethod(
    f = "plotWaterfall",
    signature = signature(object = "data.frame"),
    definition = `plotWaterfall,data.frame`
)
