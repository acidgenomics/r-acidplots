#' @name plotStackedBarPlot
#' @inherit AcidGenerics::plotStackedBarPlot
#' @note Updated 2022-03-05.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param absolute `logical(1)`.
#' Return absolute (`TRUE`) or relative/proportional (`FALSE`) cell count.
#'
#' @examples
#' data(SingleCellExperiment_Seurat, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_Seurat
#' plotStackedBarPlot(object)
NULL



## Updated 2022-03-05.
`plotStackedBarPlot,SCE` <- # nolint
    function(object,
             absolute = FALSE,
             interestingGroups = NULL,
             labels = NULL) {
        validObject(object)
        assert(isFlag(absolute))
        labels <- matchLabels(labels)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        data <- metrics(object)
        ## Generate the summary count table to pass to ggplot.
        cols <- c("interestingGroups", "ident")
        data <- data[, cols, drop = FALSE]
        ## See also our `uniteInterestingGroups()` method, which uses a
        ## similar approach internally.
        f <- as.factor(apply(
            X = as.data.frame(data),
            MARGIN = 1L,
            FUN = paste,
            collapse = ":"
        ))
        split <- split(x = data, f = f)
        assert(is(split, "SplitDFrameList"))
        data <- as.data.frame(do.call(
            what = rbind,
            args = strsplit(x = levels(f), split = ":", fixed = TRUE)
        ))
        colnames(data) <- cols
        data[["n"]] <- unname(nrow(split))
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("interestingGroups"),
                y = !!sym("n"),
                fill = !!sym("ident")
            )
        ) +
            geom_bar(
                color = "black",
                position = ifelse(
                    test = isTRUE(absolute),
                    yes = "stack",
                    no = "fill"
                ),
                stat = "identity"
            )
        ## Color palette.
        p <- p + autoDiscreteFillScale()
        ## Labels.
        if (!isSubset("x", names(labels))) {
            labels[["x"]] <- paste(interestingGroups, collapse = ":\n")
        }
        if (!isSubset("y", names(labels))) {
            labels[["y"]] <- "cell count"
            if (!isTRUE(absolute)) {
                labels[["y"]] <- paste("relative", labels[["y"]])
            }
        }
        p <- p + do.call(what = labs, args = labels)
        p
    }



#' @rdname plotStackedBarPlot
#' @export
setMethod(
    f = "plotStackedBarPlot",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotStackedBarPlot,SCE`
)
