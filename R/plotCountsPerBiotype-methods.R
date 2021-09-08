## FIXME Allow the user to custom the geom here.
##       Consider using boxplot instead of violin by default.



#' @name plotCountsPerBiotype
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotCountsPerBiotype
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
#' plotCountsPerBiotype(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' plotCountsPerBiotype(object)
NULL



## Updated 2019-12-09.
`plotCountsPerBiotype,SE` <-  # nolint
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
            sampleAxis = NULL,
            countAxis = "counts"
        )
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isInt(n),
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
        ## Determine whether to use transcripts or genes automatically.
        if (isSubset("txBiotype", colnames(rowData))) {
            biotypeCol <- "txBiotype"
        } else if (isSubset("transcriptBiotype", colnames(rowData))) {
            ## Legacy approach used until 2021-01-15.
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
            return(invisible(NULL))
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
                labels[["countAxis"]] <- paste(trans, labels[["countAxis"]])
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

formals(`plotCountsPerBiotype,SE`)[["fill"]] <-
    formalsList[["fill.discrete"]]



## Updated 2021-09-08.
`plotCountsPerBiotype,SCE` <-  # nolint
    `plotCountsPerBiotype,SE`



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBiotype",
    signature = signature("SingleCellExperiment"),
    definition = `plotCountsPerBiotype,SCE`
)

#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBiotype",
    signature = signature("SummarizedExperiment"),
    definition = `plotCountsPerBiotype,SE`
)
