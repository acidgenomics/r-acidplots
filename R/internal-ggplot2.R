## Updated 2019-08-12.
.detectMapping <- function(object) {
    assert(is(object, "ggplot"))
    ## First, check for `aes()` in `ggplot()` call.
    map <- object[["mapping"]]
    if (hasLength(map)) {
        return(map)
    }
    ## Second, check the first layer defined in the object.
    map <- object[["layers"]][[1L]][["mapping"]]
    if (hasLength(map)) {
        return(map)
    }
    abort("Failed to detect mapping.")
}



## Updated 2019-08-12.
.hideLegendsInPlotlist <- function(plotlist) {
    assert(is.list(plotlist))
    hideLegend <- function(gg) {
        gg + theme(legend.position = "none")
    }
    lapply(X = plotlist, FUN = hideLegend)
}
