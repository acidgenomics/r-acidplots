#' Flip coordinates
#'
#' Intelligently flip a plot with discrete data on the X axis.
#'
#' @export
#' @note Updated 2023-08-11.
#'
#' @details
#' This function puts the samples that were near the left origin on the X at the
#' top on the Y axis, making them more human readable.
#'
#' To my knowledge, there's not an easy way to create a `ggproto` object
#' (via CoordFlip ggproto call internally) that lets you reorder the samples
#' on the Y-axis to be reversed. So this function works directly on the
#' `ggplot` object instead of a `ggproto`, and therefore doesn't currently
#' support the `+` operator.
#'
#' @param object `ggplot`.
#'
#' @return `ggplot`.
#'
#' @seealso
#' - `coord_flip()`.
#' - `ggplot2::ggproto()`.
#' - `ggplot2:::add_ggplot`.
#' - `ggplot2:::+.gg`.
#' - https://cran.r-project.org/web/packages/ggplot2/vignettes/
#' extending-ggplot2.html
#' - https://stackoverflow.com/questions/40450904
#' - https://stackoverflow.com/questions/34227967
#' - https://github.com/tidyverse/ggplot2/blob/master/R/ggproto.r
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(data = mpg, aes(x = class)) +
#'     geom_bar()
#'
#' ## Notice the difference in Y axis sample order.
#' g + coord_flip()
#' g + acid_coord_flip(g)
acid_coord_flip <- # nolint
    function() {
        ggproto(
            `_class` = NULL,
            `_inherit` = AcidCoordFlip,
            limits = list(x = NULL, y = NULL),
            expand = TRUE,
            clip = "on"
        )
    }


## FIXME Need to import from ggplot2:
## ggproto, ggproto_parent
## CoordCartesian

## Updated 2023-08-11
AcidCoordFlip <- ggproto(
    `_class` = "CoordFlip",
    `_inherit` = CoordCartesian,
    transform = function(data, panel_params) {
        data <- .flipAxisLabels(data)
        CoordCartesian[["transform"]](data, panel_params)
    },
    backtransform_range = function(self, panel_params) {
        self[["range"]](panel_params)
    },
    range = function(self, panel_params) {
        # `summarise_layout()` expects the original x and y ranges here, not the
        # ones we would get after flipping the axes.
        unflippedRange <- ggproto_parent(CoordCartesian, self)[["range"]](panel_params)
        list("x" = unflippedRange[["y"]], "y" = unflippedRange[["x"]])
    },
    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
        parent <- ggproto_parent(CoordCartesian, self)
        panel_params <- parent$setup_panel_params(scale_x, scale_y, params)
        .flipAxisLabels(panel_params)
    },
    labels = function(labels, panel_params) {
        CoordCartesian[["labels"]](.flipAxisLabels(labels), panel_params)
    },
    setup_layout = function(layout, params) {
        # Switch the scales.
        layout[c("SCALE_X", "SCALE_Y")] <- layout[c("SCALE_Y", "SCALE_X")]
        layout
    },
    modify_scales = function(scales_x, scales_y) {
        lapply(scales_x, .scaleFlipAxis)
        lapply(scales_y, .scaleFlipAxis)
    }
)



# Maintaining the position of the `x*` and `y*` names is important for re-using
# the same `guide_transform()` as `CoordCartesian`.
.flipAxisLabels <- function(x) {
    old <- names(x)
    new <- old
    new <- gsub("^x", "z", new)
    new <- gsub("^y", "x", new)
    new <- gsub("^z", "y", new)
    setNames(x, new)
}



# In-place modification of a scale position to swap axes.
.scaleFlipAxis <- function(scale) {
    scale[["position"]] <- switch(
        scale[["position"]],
        top = "right",
        bottom = "left",
        left = "bottom",
        right = "top",
        scale[["position"]]
    )
    invisible(scale)
}
