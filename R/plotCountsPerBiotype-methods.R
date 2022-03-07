#' @name plotCountsPerBiotype
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::plotCountsPerBiotype
#' @note Updated 2022-03-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param biotypeCol `character(1)`.
#'   Biotype column name defined in `colData()`.
#' @param ... Additional arguments.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SingleCellExperiment_splatter,
#'     package = "AcidTest"
#' )
#'
#' ## SummarizedExperiment ====
#' object <- RangedSummarizedExperiment
#' plotCountsPerBiotype(object)
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' plotCountsPerBiotype(object)
NULL



## Updated 2021-09-10.
`plotCountsPerBiotype,SE` <-  # nolint
    function(
        object,
        assay = 1L,
        biotypeCol = "geneBiotype",
        n = 9L,
        interestingGroups = NULL,
        geom = c("violin", "boxplot"),
        trans = c("identity", "log2", "log10"),
        labels = list(
            "title" = "Counts per biotype",
            "subtitle" = NULL,
            "sampleAxis" = NULL,
            "countAxis" = "counts"
        )
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isString(biotypeCol),
            isInt(n)
        )
        geom <- match.arg(geom)
        trans <- match.arg(trans)
        labels <- matchLabels(labels)
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        rowData <- rowData(object)
        if (!isSubset(biotypeCol, colnames(rowData))) {
            ## nocov start
            alertWarning(sprintf(
                "{.fun %s} does not contain biotypes defined in {.val %s}.",
                "rowData", biotypeCol
            ))
            return(invisible(NULL))
            ## nocov end
        }
        rowData <- decode(rowData)
        rowData[["rowname"]] <- rownames(object)
        ## Get the top biotypes from the row data.
        biotypes <- table(rowData[[biotypeCol]])
        ## Requiring at least 10 genes per biotype.
        biotypes <- biotypes[which(biotypes > 10L)]
        biotypes <- sort(biotypes, decreasing = TRUE)
        biotypes <- head(biotypes, n = n)
        biotypes <- names(biotypes)
        ## Melt the count matrix into long format.
        data <- melt(
            object = object,
            assay = assay,
            min = 1L,
            minMethod = "perRow",
            trans = trans
        )
        data <- decode(data)
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
        )
        switch(
            EXPR = geom,
            "violin" = {
                p <- p +
                    geom_violin(
                        mapping = aes(
                            fill = str_replace_na(!!sym("interestingGroups"))
                        ),
                        color = NA,
                        scale = "area",
                        trim = TRUE
                    )
            },
            "boxplot" = {
                p <- p +
                    geom_boxplot(
                        mapping = aes(
                            color = str_replace_na(!!sym("interestingGroups"))
                        ),
                        fill = NA
                    )
            }
        )
        p <- p +
            scale_y_continuous(
                breaks = pretty_breaks(),
                labels = prettyNum
            ) +
            facet_wrap(facets = sym(biotypeCol), scales = "free_y")
        ## Labels.
        if (!identical(trans, "identity")) {
            labels[["countAxis"]] <- paste(trans, labels[["countAxis"]])
        }
        labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
        labels[["fill"]] <- labels[["color"]]
        names(labels)[names(labels) == "sampleAxis"] <- "x"
        names(labels)[names(labels) == "countAxis"] <- "y"
        p <- p + do.call(what = labs, args = labels)
        ## Color palette.
        p <- p + autoDiscreteColorScale()
        p <- p + autoDiscreteFillScale()
        ## Return.
        p
    }



## Updated 2021-09-08.
`plotCountsPerBiotype,SCE` <-  # nolint
    `plotCountsPerBiotype,SE`



## Updated 2021-09-09.
`plotCountsPerBroadClass,SE` <-  # nolint
    function(
        object,
        ...,
        labels = list(
            "title" = "Counts per broad class biotype",
            "subtitle" = NULL,
            "sampleAxis" = NULL,
            "countAxis" = "counts"
        )
    ) {
        plotCountsPerBiotype(
            object = object,
            biotypeCol = "broadClass",
            labels = matchLabels(labels),
            ...
        )
    }



`plotCountsPerBroadClass,SCE` <-  # nolint
    `plotCountsPerBroadClass,SE`



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBiotype",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotCountsPerBiotype,SCE`
)

#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBiotype",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotCountsPerBiotype,SE`
)



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBroadClass",
    signature = signature(object = "SingleCellExperiment"),
    definition = `plotCountsPerBroadClass,SCE`
)

## NOTE This is currently used in bcbioRNASeq R Markdown template.

#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBroadClass",
    signature = signature(object = "SummarizedExperiment"),
    definition = `plotCountsPerBroadClass,SE`
)
