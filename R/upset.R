#' UpSetR plot, with improved default aesthetics
#' @inherit UpSetR::upset
#' @export
upset <- UpSetR::upset

f <- formals(upset)
f[["line.size"]] <- 1L
f[["matrix.color"]] <- "black"
f[["main.bar.color"]] <- "black"
f[["matrix.dot.alpha"]] <- 1L
f[["mb.ratio"]] <- quote(c(0.6, 0.4))
f[["point.size"]] <- 3L
f[["sets.bar.color"]] <- "black"
f[["shade.alpha"]] <- 1L
f[["shade.color"]] <- NA
f[["text.scale"]] <- 1.5
formals(upset) <- f
