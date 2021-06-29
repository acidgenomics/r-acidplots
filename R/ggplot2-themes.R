## nolint start



#' ggplot2 themes
#'
#' Complete [ggplot2](http://ggplot2.tidyverse.org) themes.
#'
#' Themes are based off of `ggplot2::theme_linedraw()`, but with modifications
#' and extra user-definable parameters.
#'
#' @name themes
#' @note Updated 2019-09-13.
#'
#' @param base_size `numeric(1)`.
#'   Base font size.
#' @param base_family `character(1)`.
#'   Base font family.
#' @param face `character(1)`.
#'   Font face (`"bold"`, `"plain"`).
#' @param aspect_ratio `numeric(1)`.
#'   Aspect ratio, specifying the plot proportions. Use `1` for a perfectly
#'   square plot (including the axis labels).
#' @param legend_position `character(1)`.
#'   Legend key position. We're being a little more restrictive here, only
#'   allowing `"bottom"`, `"right"`, or `"none"`. Including the legend at the
#'   top or the left side of the plot rarely makes sense and is discouraged.
#' @param grid `logical(1)`.
#'   Label the major panel grids.
#' @param minimal `logical(1)`.
#'   Remove all axis lines, axis ticks, and panel borders.
#'
#' @return `theme`.
#'
#' @seealso
#' - `ggplot2::theme()`.
#' - https://cran.r-project.org/web/packages/ggplot2/vignettes/
#'       extending-ggplot2.html
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
#' ) +
#'     geom_point()
#'
#' ## Light theme.
#' p + acid_theme_light(legend_position = "none")
#'
#' ## Dark theme.
#' p + acid_theme_dark(legend_position = "none")
#'
#' ## Dracula theme.
#' p + acid_theme_dracula(legend_position = "none")
NULL



#' @describeIn themes Light theme that has a white background and black text.\cr
#'   Optimized for print and recommended for scientific manuscripts.\cr
#'   See `lightPalette` for color values.
#' @export
acid_theme_light <-  # nolint
    function(
        base_size = 14L,
        base_family = "",
        face = c("bold", "plain"),
        aspect_ratio = NULL,
        legend_position = c("right", "bottom", "top", "none"),
        grid = FALSE,
        minimal = FALSE
    ) {
        palette <- lightPalette
        ## Set dark mode global variable that we can access inside functions.
        options(acid.dark = FALSE)
        assert(
            isNumber(base_size),
            is.character(base_family),
            isScalar(base_family)
        )
        face <- match.arg(face)
        assert(isNumber(aspect_ratio, nullOK = TRUE))
        legend_position <- match.arg(legend_position)
        assert(
            isFlag(grid),
            isFlag(minimal)
        )
        bg <- palette[["background"]]
        fg <- palette[["foreground"]]
        border <- palette[["border"]]
        text <- element_text(
            family = base_family,
            face = face,
            color = fg
        )
        ## Include the grid lines.
        if (isTRUE(grid)) {
            panel_grid_major <- element_line(color = border, size = 0.5)
        } else {
            panel_grid_major <- element_blank()
        }
        ## Remove panel border and axis ticks.
        if (isTRUE(minimal)) {
            axis_ticks <- element_blank()
            panel_border <- element_blank()
        } else {
            axis_ticks <- element_line(color = fg)
            panel_border <- element_rect(color = fg, fill = NA)
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
                axis.text.x = element_text(
                    angle = 90L, hjust = 1L, vjust = 0.5
                ),
                axis.ticks = axis_ticks,
                panel.background = element_blank(),
                panel.border = panel_border,
                panel.grid.major = panel_grid_major,
                panel.grid.minor = element_blank(),
                legend.background = element_blank(),
                legend.position = legend_position,
                strip.background = element_rect(color = NA, fill = bg),
                strip.text = text,
                complete = TRUE,
                validate = TRUE
            )
    }



#' @describeIn themes Dark theme that has a black background and white text.\cr
#'   Inspired by `Seurat::DarkTheme()`, with some color modifications.\cr
#'   Useful for visualizing many points with a high dynamic color range, such
#'   dimension reduction plots.\cr
#'   See `darkPalette` for color values.
#' @export
acid_theme_dark <-  # nolint
    function() {
        palette <- darkPalette
        ## Set dark mode global variable that we can access inside functions.
        options(acid.dark = TRUE)
        assert(
            isNumber(base_size),
            is.character(base_family),
            isScalar(base_family)
        )
        face <- match.arg(face)
        assert(isNumber(aspect_ratio, nullOK = TRUE))
        legend_position <- match.arg(legend_position)
        assert(isFlag(grid))
        bg <- palette[["background"]]
        fg <- palette[["foreground"]]
        border <- palette[["border"]]
        text <- element_text(
            family = base_family,
            face = face,
            color = fg
        )
        ## Include the grid lines.
        if (isTRUE(grid)) {
            panel_grid_major <- element_line(color = border, size = 0.5)
        } else {
            panel_grid_major <- element_blank()
        }
        ## Remove panel border and axis ticks.
        if (isTRUE(minimal)) {
            axis_ticks <- element_blank()
            panel_border <- element_blank()
        } else {
            axis_ticks <- element_line(color = fg)
            panel_border <- element_rect(color = fg, fill = NA)
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
                axis.text.x = element_text(
                    angle = 90L, hjust = 1L, vjust = 0.5
                ),
                axis.ticks = axis_ticks,
                legend.key = element_rect(color = NA, fill = border),
                legend.background = element_blank(),
                legend.position = legend_position,
                panel.background = element_blank(),
                panel.border = panel_border,
                panel.grid.major = panel_grid_major,
                panel.grid.minor = element_blank(),
                plot.background = element_rect(color = NA, fill = bg),
                strip.text = text,
                complete = TRUE,
                validate = TRUE
            )
    }

## Set the formals.
formals(acid_theme_dark) <- formals(acid_theme_light)



#' @describeIn themes Dark theme that uses Dracula color palette.\cr
#'   See `draculaPalette` for color values.
#' @export
acid_theme_dracula <- acid_theme_dark
## Change the first line of function body to import dracula palette.
b <- body(acid_theme_dracula)
b[[2L]] <- quote(palette <- draculaPalette)
body(acid_theme_dracula) <- b



## nolint end
