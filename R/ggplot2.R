# nolint start
# Using snake case, following ggplot2 conventions.



# geoms ========================================================================
#' ggplot2 geometric objects
#'
#' Convenience functions with modified defaults for
#' [ggplot2](http://ggplot2.org).
#'
#' @section basejump_geom_abline:
#'
#' Horizontal or vertical cutoff line.
#'
#' @section basejump_geom_label:
#'
#' Modified version of [ggplot2::geom_label()].
#'
#' @section basejump_geom_label_average:
#'
#' Add average labels to a plot. For example, `col` can be `nGene`. Median or
#' mean values are always calculated per sample (`sampleName`).
#'
#' @section basejump_geom_label_repel:
#'
#' Repulsive textual annotations. Modified basejump version of
#' [ggrepel::geom_label_repel()]. If advanced customization of the text labels
#' is required, simply use the ggrepel version instead.
#'
#' @name ggplot2-geoms
#'
#' @inheritParams ggplot2::geom_label
#' @param color `character(1)`.
#'   Text color (e.g. `"orange"`).
#' @param size `integer(1)`.
#'   Font size.
#' @param xintercept,yintercept `numeric(1)`.
#'   Value denoting x- or y-axis cutoff. Specify one but not both.
#' @param data `data.frame`.
#'   Data.
#' @param col `character(1)`.
#'   Column name.
#' @param fun `character(1)`.
#'   Function name to use for average calculation. Currently supports "`mean`"
#'   or "`median`".
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
#' ## basejump_geom_abline ====
#' ## x-axis line
#' geom <- basejump_geom_abline(xintercept = 1L)
#' geom
#'
#' ## y-axis line
#' geom <- basejump_geom_abline(yintercept = 1L)
#' geom
#'
#' ## basejump_geom_label ====
#' geom <- basejump_geom_label()
#' geom
#'
#' ## basejump_geom_label_average ====
#' geom <- basejump_geom_label_average(
#'     data = tibble::tibble(
#'         sampleName = rep(c("sample1", "sample2"), times = 4L),
#'         counts = seq_len(8L)
#'     ),
#'     col = "counts",
#'     fun = "mean"
#' )
#' geom
#'
#' ## basejump_geom_label_repel ====
#' geom <- basejump_geom_label_repel()
#' geom
NULL



.geneMedianLine <- stat_summary(
    fun.y = median,
    fun.ymin = median,
    fun.ymax = median,
    geom = "crossbar",
    show.legend = FALSE,
    width = 0.5
)



.genePoint <- function(size = 3L, alpha = 1L, ...) {
    geom_point(
        size = size,
        alpha = alpha,
        position = position_jitterdodge(dodge.width = 0.9),
        ...
    )
}



#' @rdname ggplot2-geoms
#' @export
basejump_geom_abline <- function(
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
        stop("Either `xintercept` or `yintercept` is required.", call. = FALSE)
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



#' @rdname ggplot2-geoms
#' @export
basejump_geom_label <- function(
    data = NULL,
    mapping = NULL,
    ...
) {
    do.call(
        what = geom_label,
        args = list(
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
    )
}



#' @rdname ggplot2-geoms
#' @export
basejump_geom_label_average <- function(
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

    # Add `aggregate` column for facet wrapping, if necessary
    if ("aggregate" %in% colnames(data)) {
        sampleFacet <- data %>%
            .[, c("sampleName", "aggregate")] %>%
            unique()
        data <- merge(
            x = aggdata,
            y = sampleFacet,
            by = "sampleName",
            all.x = TRUE
        )
    } else {
        data <- aggdata
    }

    do.call(
        what = basejump_geom_label,
        args = list(
            data = data,
            mapping = aes(label = !!sym("roundedAverage")),
            ...
        )
    )
}



#' @rdname ggplot2-geoms
#' @export
basejump_geom_label_repel <- function(
    data = NULL,
    mapping = NULL,
    color = NULL,
    size = 4L,
    ...
) {
    geom <- do.call(
        what = geom_label_repel,
        args = list(
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
    )
    if (is.character(color)) {
        geom[["aes_params"]][["colour"]] <- color
    }
    geom
}



# themes =======================================================================
#' ggplot2 themes
#'
#' Complete [ggplot2](http://ggplot2.tidyverse.org) themes.
#'
#' All themes are based off of [ggplot2::theme_linedraw()], but with
#' modifications and extra user-definable parameters.
#'
#' @section Paperwhite:
#'
#' High contrast black and white theme optimized for print. Recommended for
#' scientific manuscripts and website tutorials.
#'
#' @section Midnight:
#'
#' Blackout theme that sets the plot background as black, with white text.
#' Inspired by `Seurat::DarkTheme()`, with some color modifications. Useful
#' for visualizing many points with a high dynamic color range, such as t-SNE
#' expression plots.
#'
#' @name ggplot2-themes
#'
#' @param base_size `numeric(1)`. Base font size.
#' @param base_family `character(1)`. Base font family.
#' @param face `character(1)`. Font face ("`bold`", "`plain`").
#' @param aspect_ratio `numeric(1)`. Aspect ratio, specifying the plot
#'   proportions. Use `1` for a perfectly square plot (including the axis
#'   labels).
#' @param legend_position `character(1)`. Legend key position. We're being a
#'   little more restrictive here, only allowing "`bottom`", "`right`", or
#'   "`none`". Including the legend at the top or the left side of the plot
#'   rarely makes sense and is discouraged.
#' @param grid `logical(1)`. Label the major panel grids with a gray accent.
#' @param minimal `logical(1)`. Remove all axis lines, axis ticks, and
#'   panel borders.
#'
#' @return `theme`.
#'
#' @seealso `ggplot2::theme()`.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(
#'     data = mpg,
#'     mapping = aes(
#'         x = manufacturer,
#'         y = displ,
#'         color = manufacturer,
#'         fill = manufacturer
#'     )
#' )
#'
#' ## Paperwhite theme.
#' p +
#'     geom_point() +
#'     theme_paperwhite(legend_position = "none")
#'
#' ## Midnight theme.
#' p +
#'     geom_point() +
#'     theme_midnight(legend_position = "none")
NULL



#' @rdname ggplot2-themes
#' @export
theme_paperwhite <- function(
    base_size = 14L,
    base_family = "",
    face = c("bold", "plain"),
    aspect_ratio = NULL,
    legend_position = c("right", "bottom", "top", "none"),
    grid = FALSE,
    minimal = FALSE
) {
    assert(
        isNumber(base_size),
        # Don't use `isString()` check on `base_family`, since empty string is
        # allowed by ggplot2.
        is.character(base_family) && length(base_family) == 1L
    )
    face <- match.arg(face)
    assert(isNumber(aspect_ratio, nullOK = TRUE))
    legend_position <- match.arg(legend_position)
    assert(
        isFlag(grid),
        isFlag(minimal)
    )

    gray <- "gray95"

    text <- element_text(
        family = base_family,
        face = face,
        colour = "black"
    )

    # Include the grid lines.
    if (isTRUE(grid)) {
        panel_grid_major <- element_line(colour = gray, size = 0.5)
    } else {
        panel_grid_major <- element_blank()
    }

    # Remove panel border and axis ticks.
    if (isTRUE(minimal)) {
        axis_ticks <- element_blank()
        panel_border <- element_blank()
    } else {
        axis_ticks <- element_line(colour = "black")
        panel_border <- element_rect(colour = "black", fill = NA)
    }

    theme_linedraw(
        base_size = base_size,
        base_family = base_family
    ) +
        theme(
            text = text,
            aspect.ratio = aspect_ratio,
            axis.line = element_blank(),
            axis.text = text,
            axis.text.x = element_text(angle = 90L, hjust = 1L, vjust = 0.5),
            axis.ticks = axis_ticks,
            panel.background = element_blank(),
            panel.border = panel_border,
            panel.grid.major = panel_grid_major,
            panel.grid.minor = element_blank(),
            legend.background = element_blank(),
            legend.position = legend_position,
            strip.background = element_rect(colour = NA, fill = "white"),
            strip.text = text,
            complete = TRUE,
            validate = TRUE
        )
}



#' @rdname ggplot2-themes
#' @export
theme_midnight <- function() {
    assert(
        isNumber(base_size),
        # Don't use `isString()` check on `base_family`, since empty string is
        # allowed by ggplot2.
        is.character(base_family) && length(base_family) == 1L
    )
    face <- match.arg(face)
    assert(isNumber(aspect_ratio, nullOK = TRUE))
    legend_position <- match.arg(legend_position)
    assert(isFlag(grid))

    gray <- "gray10"

    text <- element_text(
        family = base_family,
        face = face,
        colour = "white"
    )

    # Include the grid lines.
    if (isTRUE(grid)) {
        panel_grid_major <- element_line(colour = gray, size = 0.5)
    } else {
        panel_grid_major <- element_blank()
    }

    # Remove panel border and axis ticks.
    if (isTRUE(minimal)) {
        axis_ticks <- element_blank()
        panel_border <- element_blank()
    } else {
        axis_ticks <- element_line(colour = "white")
        panel_border <- element_rect(colour = "white", fill = NA)
    }

    theme_linedraw(
        base_size = base_size,
        base_family = base_family
    ) +
        theme(
            text = text,
            aspect.ratio = aspect_ratio,
            axis.line = element_blank(),
            axis.text = text,
            axis.text.x = element_text(angle = 90L, hjust = 1L, vjust = 0.5),
            axis.ticks = axis_ticks,
            legend.key = element_rect(colour = NA, fill = gray),
            legend.background = element_blank(),
            legend.position = legend_position,
            panel.background = element_blank(),
            panel.border = panel_border,
            panel.grid.major = panel_grid_major,
            panel.grid.minor = element_blank(),
            plot.background = element_rect(colour = NA, fill = "black"),
            strip.text = text,
            complete = TRUE,
            validate = TRUE
        )
}

# Set the formals.
formals(theme_midnight) <- formals(theme_paperwhite)



# nolint end
