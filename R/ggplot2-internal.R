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
