.detectMapping <- function(object) {
    assert(is(object, "ggplot"))

    ## First, check for `aes()` in `ggplot()` call.
    map <- object[["mapping"]]
    if (hasLength(map)) return(map)

    ## Second, check the first layer defined in the object.
    map <- object[["layers"]][[1L]][["mapping"]]
    if (hasLength(map)) return(map)

    stop("Failed to detect mapping.")
}
