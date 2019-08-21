#' @name plotCountsPerBroadClass
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::plotCountsPerBroadClass
#' @note Updated 2019-08-21.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment,
#'     package = "acidtest"
#' )
#' rse <- RangedSummarizedExperiment
#' sce <- SingleCellExperiment
#'
#' ## SummarizedExperiment ====
#' plotCountsPerBroadClass(rse)
#'
#' ## SingleCellExperiment ====
#' plotCountsPerBroadClass(sce)
NULL



#' @rdname plotCountsPerBroadClass
#' @name plotCountsPerBroadClass
#' @importFrom bioverbs plotCountsPerBroadClass
#' @usage plotCountsPerBroadClass(object, ...)
#' @export
NULL



## Updated 2019-08-21.
`plotCountsPerBroadClass,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        trans = c("identity", "log2", "log10"),
        fill,
        countsAxisLabel = "counts",
        title = "Counts per broad class"
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isString(countsAxisLabel),
            isString(title, nullOK = TRUE)
        )
        trans <- match.arg(trans)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        if (trans != "identity") {
            countsAxisLabel <- paste(trans, countsAxisLabel)
        }

        ## Melt the count matrix into long format.
        data <- meltCounts(
            object = object,
            assay = assay,
            minCounts = 1L,
            trans = trans
        )
        data <- decode(data)

        ## Get the row data and prepare for left join via "rowname" column.
        rowData <- rowData(object)
        rowData <- decode(rowData)
        rowData[["rowname"]] <- rownames(object)

        biotypeCol <- "broadClass"
        ## Warn and early return if the biotypes are not defined in rowData.
        if (!biotypeCol %in% colnames(rowData)) {
            ## nocov start
            warning(sprintf(
                "'rowData()' does not contain biotypes defined in '%s' column.",
                biotypeCol
            ))
            return(invisible())
            ## nocov end
        }

        ## Get the top biotypes from the row data.
        biotypes <- table(rowData[[biotypeCol]])
        ## Requiring at least 10 genes per biotype.
        biotypes <- biotypes[which(biotypes > 10L)]
        biotypes <- sort(biotypes, decreasing = TRUE)
        biotypes <- names(biotypes)

        ## Prepare the minimal data frame required for plotting.
        data <- left_join(x = data, y = rowData, by = "rowname")
        keep <- which(data[[biotypeCol]] %in% biotypes)
        data <- data[keep, , drop = FALSE]
        data <- data[, c("counts", "interestingGroups", biotypeCol)]
        ## Sanitize the biotype column to appear nicer in plots.
        data[[biotypeCol]] <- gsub("_", " ", data[[biotypeCol]])

        ## Plot.
        data <- as_tibble(data, rownames = NULL)
        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("interestingGroups"),
                y = !!sym("counts")
            )
        ) +
            geom_violin(
                mapping = aes(fill = !!sym("interestingGroups")),
                color = NA,
                scale = "area",
                trim = TRUE
            ) +
            scale_y_continuous(
                breaks = pretty_breaks(),
                labels = prettyNum
            ) +
            facet_wrap(facets = sym(biotypeCol), scales = "free_y") +
            labs(
                title = title,
                x = NULL,
                y = countsAxisLabel,
                fill = paste(interestingGroups, collapse = ":\n")
            ) +
            theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank()
            )
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
