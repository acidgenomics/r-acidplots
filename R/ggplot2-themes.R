# nolint start
# Using snake case, following ggplot2 conventions.



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
#' @name acid_theme
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



#' @rdname acid_theme
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



#' @rdname acid_theme
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

    # Set dark mode global variable that we can access in plotting functions.
    options(acid.dark = TRUE)

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
