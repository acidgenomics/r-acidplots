#' ggplot2 geometric objects
#'
#' Convenience functions with modified defaults for
#' [ggplot2](http://ggplot2.org).
#'
#' @section acid_geom_abline:
#'
#' Horizontal or vertical cutoff line.
#'
#' @section acid_geom_label:
#'
#' Modified version of [ggplot2::geom_label()].
#'
#' @section acid_geom_label_average:
#'
#' Add average labels to a plot. For example, `col` can be `nGene`. Median or
#' mean values are always calculated per sample (`sampleName`).
#'
#' @section acid_geom_label_repel:
#'
#' Repulsive textual annotations. Modified basejump version of
#' [ggrepel::geom_label_repel()]. If advanced customization of the text labels
#' is required, simply use the ggrepel version instead.
#'
#' @name geoms
#' @note Updated 2019-08-21.
#'
#' @inheritParams ggplot2::geom_label
#' @param color `character(1)`.
#'   Text color (e.g. orange).
#' @param size `integer(1)`.
#'   Font size.
#' @param xintercept,yintercept `numeric(1)`.
#'   Value denoting x- or y-axis cutoff. Specify one but not both.
#' @param data `data.frame`.
#'   Data.
#' @param col `character(1)`.
#'   Column name.
#' @param fun `character(1)`.
#'   Function name to use for average calculation.
#'   Currently supports mean or median.
#' @param digits `integer(1)`.
#'   Number of significant digits to use. Defaults to rounded.
#'
#' @seealso
#' - `ggplot2::geom_label()`.
#' - `ggrepel::geom_label_repel()`.
#'
#' @return `ggproto`.
#'
#' @examples
#' ## acid_geom_abline ====
#' ## x-axis line
#' geom <- acid_geom_abline(xintercept = 1L)
#' geom
#'
#' ## y-axis line
#' geom <- acid_geom_abline(yintercept = 1L)
#' geom
#'
#' ## acid_geom_label ====
#' geom <- acid_geom_label()
#' geom
#'
#' ## acid_geom_label_average ====
#' data = data.frame(
#'     sampleName = rep(c("sample1", "sample2"), times = 4L),
#'     counts = seq_len(8L)
#' )
#' geom <- acid_geom_label_average(
#'     data = data,
#'     col = "counts",
#'     fun = "mean"
#' )
#' geom
#'
#' ## acid_geom_label_repel ====
#' geom <- acid_geom_label_repel()
#' geom
NULL



#' @rdname geoms
#' @export
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



#' @rdname geoms
#' @export
acid_geom_bar <-  # nolint
    function(..., color = NA, stat = "identity") {
        geom_bar(..., color = NA, stat = "identity")
    }



#' @rdname geoms
#' @export
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



#' @rdname geoms
#' @export
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



#' @rdname geoms
#' @export
acid_geom_label_repel <-  # nolint
    function(
        data = NULL,
        mapping = NULL,
        color = NULL,
        size = 4L,
        ...
    ) {
        geom <- geom_label_repel(
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
