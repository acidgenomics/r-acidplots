#' @name plotCountsPerBroadClass
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotCountsPerBroadClass
#' @note Updated 2019-12-09.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotCountsPerBroadClass(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotCountsPerBroadClass(object)
NULL



#' @rdname plotCountsPerBroadClass
#' @name plotCountsPerBroadClass
#' @importFrom AcidGenerics plotCountsPerBroadClass
#' @usage plotCountsPerBroadClass(object, ...)
#' @export
NULL



## Updated 2019-12-09.
`plotCountsPerBroadClass,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        trans = c("identity", "log2", "log10"),
        fill,
        labels = list(
            title = "Counts per broad class",
            subtitle = NULL,
            sampleAxis = NULL,
            countAxis = "counts"
        )
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE)
        )
        trans <- match.arg(trans)
        labels <- matchLabels(
            labels = labels,
            choices = eval(formals()[["labels"]])
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        ## Melt the count matrix into long format.
        data <- melt(
            object = object,
            assay = assay,
            min = 1L,
            minMethod = "perRow",
            trans = trans
        )
        data <- decode(data)
        ## Get the row data and prepare for left join via "rowname" column.
        rowData <- rowData(object)
        rowData <- decode(rowData)
        rowData[["rowname"]] <- rownames(object)
        biotypeCol <- "broadClass"
        ## Warn and early return if the biotypes are not defined in rowData.
        if (!isSubset(biotypeCol, colnames(rowData))) {
            ## nocov start
            warning(sprintf(
                "'rowData()' does not contain biotypes defined in '%s' column.",
                biotypeCol
            ))
            return(invisible(NULL))
            ## nocov end
        }
        ## Get the top biotypes from the row data.
        biotypes <- table(rowData[[biotypeCol]])
        ## Requiring at least 10 genes per biotype.
        biotypes <- biotypes[which(biotypes > 10L)]
        biotypes <- sort(biotypes, decreasing = TRUE)
        biotypes <- names(biotypes)
        ## Prepare the minimal data frame required for plotting.
        data <- leftJoin(x = data, y = rowData, by = "rowname")
        keep <- which(data[[biotypeCol]] %in% biotypes)
        data <- data[keep, , drop = FALSE]
        data <- data[, c("value", "interestingGroups", biotypeCol)]
        ## Sanitize the biotype column to appear nicer in plots.
        data[[biotypeCol]] <- gsub("_", " ", data[[biotypeCol]])
        ## Plot.
        data <- as_tibble(data, rownames = NULL)
        p <- ggplot(
            data = data,
            mapping = aes(
                x = str_replace_na(!!sym("interestingGroups")),
                y = !!sym("value")
            )
        ) +
            geom_violin(
                mapping = aes(
                    fill = str_replace_na(!!sym("interestingGroups"))
                ),
                color = NA,
                scale = "area",
                trim = TRUE
            ) +
            scale_y_continuous(
                breaks = pretty_breaks(),
                labels = prettyNum
            ) +
            facet_wrap(facets = sym(biotypeCol), scales = "free_y")
        ## Labels.
        if (is.list(labels)) {
            if (!identical(trans, "identity")) {
                labels[["y"]] <- paste(trans, labels[["y"]])
            }
            labels[["fill"]] <- paste(interestingGroups, collapse = ":\n")
            names(labels)[names(labels) == "sampleAxis"] <- "x"
            names(labels)[names(labels) == "countAxis"] <- "y"
            p <- p + do.call(what = labs, args = labels)
        }
        ## Fill.
        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }
        ## Return.
        p
    }

formals(`plotCountsPerBroadClass,SummarizedExperiment`)[["fill"]] <-
    formalsList[["fill.discrete"]]



#' @rdname plotCountsPerBroadClass
#' @export
setMethod(
    f = "plotCountsPerBroadClass",
    signature = signature("SummarizedExperiment"),
    definition = `plotCountsPerBroadClass,SummarizedExperiment`
)
