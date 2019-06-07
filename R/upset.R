#' UpSetR plot, with improved default aesthetics
#' @export
#' @inherit UpSetR::upset description details params references
#' @return Graphical output, no return.
#' @examples
#' ## Refer to UpSetR::upset() documentation for more examples.
#' movies <- read.csv(
#'     system.file("extdata", "movies.csv", package = "UpSetR"),
#'     header = TRUE,
#'     sep = ";"
#' )
#' upset(movies)
# Last modified 2019-06-07.
upset <- UpSetR::upset

f <- formals(upset)
f[["line.size"]] <- 1L
f[["main.bar.color"]] <- "black"
f[["matrix.color"]] <- "black"
f[["matrix.dot.alpha"]] <- 1L
f[["mb.ratio"]] <- quote(c(0.6, 0.4))
f[["point.size"]] <- 3L
f[["sets.bar.color"]] <- "black"
f[["shade.alpha"]] <- 1L
f[["shade.color"]] <- NA
f[["text.scale"]] <- 1.5
# Fix T/F usage in UpSetR package.
f[["decreasing"]] <- c(TRUE, FALSE)
f[["keep.order"]] <- FALSE
formals(upset) <- f
