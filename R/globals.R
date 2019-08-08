globalVariables(".")

.version <- packageVersion("acidplots")

#' acidplots test data URL
#' @keywords internal
#' @export
#' @examples
#' acidplotsTestsURL
acidplotsTestsURL <- paste0(
    "http://tests.acidgenomics.com/acidplots/",
    "v", .version$major, ".", .version$minor  # nolint
)

## This is also defined in acidplots.
geom <- c("histogram", "ecdf", "violin", "ridgeline", "boxplot")
