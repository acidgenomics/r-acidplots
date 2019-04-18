.detectMapping <- function(object) {
    assert(is(object, "ggplot"))

    # First, check for `aes()` in `ggplot()` call.
    map <- object[["mapping"]]
    if (hasLength(map)) return(map)

    # Second, check the first layer defined in the object.
    map <- object[["layers"]][[1L]][["mapping"]]
    if (hasLength(map)) return(map)

    stop("Failed to detect mapping.")
}



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
