globalVariables(".")

packageVersion <- packageVersion("acidplots")

#' acidplots test data URL
#' @keywords internal
#' @export
#' @examples
#' acidplotsTestsURL
acidplotsTestsURL <- paste0(
    "http://tests.acidgenomics.com/acidplots/",
    "v", packageVersion$major, ".", packageVersion$minor  # nolint
)
