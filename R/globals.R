globalVariables(".")

packageVersion <- packageVersion("acidplots")

#' freerange test data URL
#' @keywords internal
#' @export
#' @examples
#' minimalismTestsURL
minimalismTestsURL <- paste0(
    "http://tests.acidgenomics.com/acidplots/",
    "v", packageVersion$major, ".", packageVersion$minor  # nolint
)

formalsList <- c(
    formalsList,
    list(
        acid.heatmap.color = quote(
            getOption("acid.heatmap.color", default = synesthesia)
        )
    )
)
