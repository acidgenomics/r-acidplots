globalVariables(".")

packageVersion <- packageVersion("minimalism")

#' freerange test data URL
#' @keywords internal
#' @export
#' @examples
#' minimalismTestsURL
minimalismTestsURL <- paste0(
    "http://tests.acidgenomics.com/minimalism/",
    "v", packageVersion$major, ".", packageVersion$minor  # nolint
)
