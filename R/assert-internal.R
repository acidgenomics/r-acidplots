## Consider moving this to goalie.
## Updated 2019-08-12.
.hasAggregate <- function(x) {
    assert(is.data.frame(x))
    "aggregate" %in% colnames(x)
}
