#' @name plotCountsPerBiotype
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::plotCountsPerBiotype
#' @note Updated 2019-09-16.
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
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotCountsPerBiotype(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotCountsPerBiotype(object)
NULL



#' @rdname plotCountsPerBiotype
#' @name plotCountsPerBiotype
#' @importFrom bioverbs plotCountsPerBiotype
#' @usage plotCountsPerBiotype(object, ...)
#' @export
NULL



## Updated 2019-09-16.
`plotCountsPerBiotype,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        n = 9L,
        interestingGroups = NULL,
        trans = c("identity", "log2", "log10"),
        fill,
        labels = list(
            title = "Counts per biotype",
            subtitle = NULL,
            samplesAxis = NULL,
            countsAxis = "counts"
        )
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isInt(n),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE)
        )
        trans <- match.arg(trans)
        labels <- .labels(
            labels = labels,
            labelsArgs = eval(formals()[["labels"]])
        )
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        ## Melt the count matrix into long format.
        data <- melt(
            object = object,
            assay = assay,
            min = 1L,
            trans = trans
        )
        data <- decode(data)
        ## Get the row data and prepare for left join via "rowname" column.
        rowData <- rowData(object)
        rowData <- decode(rowData)
        rowData[["rowname"]] <- rownames(object)
        ## Determine whether to use transcripts or genes automatically.
        if (isSubset("transcriptBiotype", colnames(rowData))) {
            biotypeCol <- "transcriptBiotype"
        } else {
            biotypeCol <- "geneBiotype"
        }
        ## Warn and early return if the biotypes are not defined in rowData.
        if (!isSubset(biotypeCol, colnames(rowData))) {
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
        biotypes <- head(biotypes, n = n)
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
                x = !!sym("interestingGroups"),
                y = !!sym("value")
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
            facet_wrap(facets = sym(biotypeCol), scales = "free_y")
        ## Labels.
        if (is.list(labels)) {
            if (!identical(trans, "identity")) {
                labels[["y"]] <- paste(trans, labels[["y"]])
            }
            labels[["fill"]] <- paste(interestingGroups, collapse = ":\n")
            names(labels)[names(labels) == "samplesAxis"] <- "x"
            names(labels)[names(labels) == "countsAxis"] <- "y"
            p <- p + do.call(what = labs, args = labels)
        }
        ## Fill.
        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }
        ## Return.
        p
    }

formals(`plotCountsPerBiotype,SummarizedExperiment`)[["fill"]] <-
    formalsList[["fill.discrete"]]



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBiotype",
    signature = signature("SummarizedExperiment"),
    definition = `plotCountsPerBiotype,SummarizedExperiment`
)



## Updated 2019-08-20.
`plotCountsPerBiotype,SingleCellExperiment` <-  # nolint
    `plotCountsPerBiotype,SummarizedExperiment`



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBiotype",
    signature = signature("SingleCellExperiment"),
    definition = `plotCountsPerBiotype,SingleCellExperiment`
)
