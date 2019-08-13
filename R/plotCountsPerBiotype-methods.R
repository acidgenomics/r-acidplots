#' @name plotCountsPerBiotype
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::plotCountsPerBiotype
#' @note Updated 2019-07-29.
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
#' plotCountsPerBiotype(rse)
#'
#' ## SingleCellExperiment ====
#' plotCountsPerBiotype(sce)
NULL



#' @rdname plotCountsPerBiotype
#' @name plotCountsPerBiotype
#' @importFrom bioverbs plotCountsPerBiotype
#' @usage plotCountsPerBiotype(object, ...)
#' @export
NULL



## Updated 2019-07-29.
`plotCountsPerBiotype,SummarizedExperiment` <-  # nolint
    function(
        object,
        assay = 1L,
        n = 9L,
        interestingGroups = NULL,
        trans = c("identity", "log2", "log10"),
        fill,
        countsAxisLabel = "counts",
        title = "Counts per biotype"
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isInt(n),
            isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
            isString(countsAxisLabel),
            isString(title, nullOK = TRUE)
        )
        trans <- match.arg(trans)

        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)

        ## Get the count matrix.
        assay <- assays(object)[[assay]]
        ## Ensure sparse matrix is coerced to dense.
        assay <- as.matrix(assay)

        ## Log transform, if necessary.
        if (trans == "log2") {
            assay <- log2(assay + 1L)
        } else if (trans == "log10") {
            assay <- log10(assay + 1L)
        }
        if (trans != "identity") {
            countsAxisLabel <- paste(trans, countsAxisLabel)
        }

        rowData <- rowData(object)
        ## Ensure Rle columns get decoded.
        rowData <- decode(rowData)
        ## Ensure row names are defined, which isn't always the case for
        ## row data derived from SE (non-ranged).
        rownames(rowData) <- rownames(object)

        ## Determine whether to use transcripts or genes automatically.
        if ("transcriptBiotype" %in% colnames(rowData)) {
            biotypeCol <- "transcriptBiotype"
        } else {
            biotypeCol <- "geneBiotype"
        }

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
        biotypes <- rowData %>%
            as_tibble() %>%
            select(!!sym(biotypeCol)) %>%
            group_by(!!sym(biotypeCol)) %>%
            summarise(n = n()) %>%
            ## Require at least 10 genes.
            filter(!!sym("n") >= 10L) %>%
            arrange(desc(!!sym("n"))) %>%
            top_n(n = !!n, wt = !!sym("n")) %>%
            pull(!!sym(biotypeCol)) %>%
            as.character()

        ## Coerce the sample data to a tibble.
        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID") %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID")))

        ## Gather the counts into a long tibble.
        data <- assay %>%
            as_tibble(rownames = "rowname") %>%
            gather(
                key = "colname",
                value = "counts",
                -UQ(sym("rowname"))
            )

        ## SingleCellExperiment requires cell2sample mapping.
        if (is(object, "SingleCellExperiment")) {
            c2s <- cell2sample(object, return = "tibble") %>%
                rename(!!sym("colname") := !!sym("cellID"))
            data <- left_join(
                x = as_tibble(data),
                y = as_tibble(c2s),
                by = "colname"
            )
        } else {
            data <- rename(data, !!sym("sampleID") := !!sym("colname"))
        }

        ## Prepare the minimal tibble required for plotting.
        data <- data %>%
            filter(!!sym("counts") > 0L) %>%
            left_join(
                y = as_tibble(rowData, rownames = "rowname"),
                by = "rowname"
            ) %>%
            filter(!!sym(biotypeCol) %in% !!biotypes) %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID"))) %>%
            left_join(
                y = as_tibble(sampleData),
                by = "sampleID"
            ) %>%
            select(!!!syms(c("counts", "interestingGroups", biotypeCol))) %>%
            mutate(!!sym(biotypeCol) := gsub("_", " ", !!sym(biotypeCol)))

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

        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }

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
