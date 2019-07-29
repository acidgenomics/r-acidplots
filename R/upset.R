#' UpSetR plot
#'
#' Wrapper for [UpSetR::upset()] with improved default aesthetics.
#'
#' @inherit UpSetR::upset details params references
#' @note Updated 2019-07-29.
#' @export
#'
#' @inheritParams params
#'
#' @return Graphical output, no return.
#'
#' @examples
#' ## Refer to UpSetR::upset() documentation for more examples.
#' movies <- read.csv(
#'     system.file("extdata", "movies.csv", package = "UpSetR"),
#'     header = TRUE,
#'     sep = ";"
#' )
#' upset(movies)
upset <- function(
    ...,
    ## nolint start
    line.size = 1L,
    main.bar.color = "black",
    matrix.color = "black",
    matrix.dot.alpha = 1L,
    mb.ratio = c(0.6, 0.4),
    point.size = 3L,
    sets.bar.color = "black",
    shade.alpha = 1L,
    shade.color = NA,
    text.scale = 1.5,
    ## Fix T/F usage in UpSetR package.
    decreasing = c(TRUE, FALSE),
    keep.order = FALSE
    ## nolint end
) {
    UpSetR::upset(
        ...,
        line.size = line.size,
        main.bar.color = main.bar.color,
        matrix.color = matrix.color,
        matrix.dot.alpha = matrix.dot.alpha,
        mb.ratio = mb.ratio,
        point.size = point.size,
        sets.bar.color = sets.bar.color,
        shade.alpha = shade.alpha,
        shade.color = shade.color,
        text.scale = text.scale,
        decreasing = decreasing,
        keep.order = keep.order
    )
}
