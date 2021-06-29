#' Reference line
#'
#' Add a horizontal (using `yintercept`) or vertical (using `xintercept`)
#' reference line.
#'
#' @export
#' @note Updated 2021-06-29.
#'
#' @param xintercept,yintercept `numeric(1)`.
#'   Value denoting x- or y-axis cutoff.
#'   Use either `xintercept` or `yintercept`, but not both.
#'
#' @return `Layer`/`ggproto`.
#'
#' @examples
#' ## x-axis line.
#' geom <- acid_geom_abline(xintercept = 1L)
#' print(geom)
#'
#' ## y-axis line.
#' geom <- acid_geom_abline(yintercept = 1L)
#' print(geom)
acid_geom_abline <-  # nolint
    function(
        xintercept = NULL,
        yintercept = NULL
    ) {
        alpha <- 0.75
        color <- "black"
        linetype <- "dashed"
        size <- 1L
        if (
            (is.null(xintercept) && is.null(yintercept)) ||
            (is.numeric(xintercept) && is.numeric(yintercept))
        ) {
            stop("Either 'xintercept' or 'yintercept' is required.")
        } else if (is.numeric(xintercept)) {
            geom_vline(
                xintercept = xintercept,
                alpha = alpha,
                color = color,
                linetype = linetype,
                size = size
            )
        } else if (is.numeric(yintercept)) {
            geom_hline(
                yintercept = yintercept,
                alpha = alpha,
                color = color,
                linetype = linetype,
                size = size
            )
        }
    }



#' Bar chart
#'
#' Modified version of `ggplot2::geom_bar()`.
#'
#' @export
#' @note Updated 2021-06-29.
#'
#' @param ...
#'   Additional arguments, passed to `ggplot2::geom_bar()`.
#' @param color `character(1)`.
#'   Line color.
#'   Defaults to disabled, using `NA`.
#' @param stat `character(1)`.
#'   Statistical transformation to use on the data for this layer.
#'
#' @return `Layer`/`ggproto`.
#'
#' @examples
#' data <- data.frame(
#'     x = c("a", "b", "c", "d"),
#'     y = c(5L, 10L, 15L, 20L)
#' )
#' p <- ggplot(data = data, mapping = aes(x = !!sym("x"), y = !!sym("y")))
#' p + acid_geom_bar(fill = "black")
acid_geom_bar <-  # nolint
    function(..., color = NA, stat = "identity") {
        geom_bar(..., color = color, stat = stat)
    }



#' Text label
#'
#' Modified version of `ggplot2::geom_label()`.
#'
#' @export
#' @note Updated 2021-06-29.
#'
#' @param data `data.frame`.
#'   Data frame, containing plot data.
#' @param mapping
#'   Set of aesthetic mappings created by `ggplot2::aes()`.
#' @param ...
#'   Additional arguments, passed to `ggplot2::geom_label()`.
#'
#' @return `Layer`/`ggproto`.
#'
#' @examples
#' geom <- acid_geom_label()
#' print(geom)
acid_geom_label <-  # nolint
    function(
        data = NULL,
        mapping = NULL,
        ...
    ) {
        geom_label(
            data = data,
            mapping = mapping,
            alpha = 0.75,
            color = "white",
            fill = "black",
            fontface = "bold",
            label.padding = unit(0.2, "lines"),
            label.size = NA,
            show.legend = FALSE,
            ...
        )
    }



#' Average labels
#'
#' Add average labels to a plot.
#'
#' @details
#' For example, `col` can be `nGene`. Median or mean values are always
#' calculated per sample (`sampleName`).
#'
#' @export
#' @note Updated 2021-06-29.
#'
#' @param data `data.frame`.
#'   Data frame, containing plot data.
#' @param col `character(1)`.
#'   Column name.
#' @param fun `character(1)`.
#'   Function name to use for average calculation.
#'   Currently supports mean or median.
#' @param digits `integer(1)`.
#'   Number of significant digits to use.
#'   Defaults to rounded.
#' @param ...
#'   Additional arguments, passed to [acid_geom_label()].
#'
#' @return `Layer`/`ggproto`.
#'
#' @examples
#' data = data.frame(
#'     "sampleName" = rep(c("sample1", "sample2"), times = 4L),
#'     "counts" = seq_len(8L)
#' )
#' geom <- acid_geom_label_average(
#'     data = data,
#'     col = "counts",
#'     fun = "mean"
#' )
#' print(geom)
acid_geom_label_average <-  # nolint
    function(
        data,
        col,
        fun = c("mean", "median"),
        digits = 0L,
        ...
    ) {
        data <- as.data.frame(data)
        assert(
            isString(col),
            isSubset(col, colnames(data)),
            isInt(digits)
        )
        fun <- match.arg(fun)
        fun <- get(fun)
        assert(is.function(fun))
        aggdata <- aggregate(
            formula = as.formula(paste(col, "sampleName", sep = " ~ ")),
            data = data,
            FUN = fun
        )
        aggdata[["roundedAverage"]] <- round(aggdata[[col]], digits = digits)
        ## Add `aggregate` column for facet wrapping, if necessary
        if (isSubset("aggregate", colnames(data))) {
            sampleFacet <- unique(data[, c("sampleName", "aggregate")])
            data <- merge(
                x = aggdata,
                y = sampleFacet,
                by = "sampleName",
                all.x = TRUE
            )
        } else {
            data <- aggdata
        }
        acid_geom_label(
            data = data,
            mapping = aes(label = !!sym("roundedAverage")),
            ...
        )
    }



#' Repulsive textual annotations
#'
#' Modified version of `ggrepel::geom_label_repel()`.
#'
#' @details
#' If advanced customization of the text labels is required, simply use the
#' ggrepel version instead.
#'
#' @export
#' @note Updated 2021-06-29.
#'
#' @param data `data.frame`.
#'   Data frame, containing plot data.
#' @param mapping
#'   Set of aesthetic mappings created by `ggplot2::aes()`.
#' @param color `character(1)` or `NULL`.
#'   Text color.
#' @param size `integer(1)`.
#'   Font size.
#' @param ...
#'   Additional arguments, passed to `ggrepel::geom_label_repel()`.
#'
#' @return `Layer`/`ggproto`.
#'
#' @examples
#' geom <- acid_geom_label_repel()
#' print(geom)
acid_geom_label_repel <-  # nolint
    function(
        data = NULL,
        mapping = NULL,
        color = NULL,
        size = 4L,
        ...
    ) {
        requireNamespaces("ggrepel")
        geom <- ggrepel::geom_label_repel(
            data = data,
            mapping = mapping,
            arrow = arrow(length = unit(0.01, "npc")),
            box.padding = unit(0.5, "lines"),
            fill = "white",
            fontface = "bold",
            force = 1L,
            point.padding = unit(0.75, "lines"),
            segment.size = 0.5,
            show.legend = FALSE,
            size = size,
            ...
        )
        if (is.character(color)) {
            geom[["aes_params"]][["colour"]] <- color
        }
        geom
    }
