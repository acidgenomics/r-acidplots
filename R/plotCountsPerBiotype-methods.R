#' @name plotCountsPerBiotype
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::plotCountsPerBiotype
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(rse, sce, package = "acidtest")
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



plotCountsPerBiotype.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        n = 9L,
        interestingGroups = NULL,
        trans = c("log10", "log2", "identity"),
        countsAxisLabel = "counts",
        title = "Counts per biotype"
    ) {
        validObject(object)
        assert(
            isScalar(assay),
            isInt(n),
            isString(countsAxisLabel),
            isString(title, nullOK = TRUE)
        )
        trans <- match.arg(trans)
        breaks <- switch(
            EXPR = trans,
            identity = waiver(),
            log2 = log_breaks(base = 2L),
            log10 = log_breaks(base = 10L)
        )

        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)

        rowData <- rowData(object)
        # Ensure Rle columns get decoded.
        rowData <- decode(rowData)
        # Ensure row names are defined, which isn't always the case for
        # row data derived from SE (non-ranged).
        rownames(rowData) <- rownames(object)

        # Determine whether to use transcripts or genes automatically.
        if ("transcriptBiotype" %in% colnames(rowData)) {
            biotypeCol <- "transcriptBiotype"
        } else {
            biotypeCol <- "geneBiotype"
        }

        # Warn and early return if the biotypes are not defined in rowData.
        if (!biotypeCol %in% colnames(rowData)) {
            warning(paste(
                "rowData() does not contain biotypes defined in",
                biotypeCol, "column."
            ))
            return(invisible())
        }

        # Get the top biotypes from the row data.
        biotypes <- rowData %>%
            as_tibble() %>%
            select(!!sym(biotypeCol)) %>%
            group_by(!!sym(biotypeCol)) %>%
            summarise(n = n()) %>%
            # Require at least 10 genes.
            filter(!!sym("n") >= 10L) %>%
            arrange(desc(!!sym("n"))) %>%
            top_n(n = n, wt = !!sym("n")) %>%
            pull(!!sym(biotypeCol)) %>%
            as.character()

        # Coerce the sample data to a tibble.
        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID") %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID")))

        # Gather the counts into a long tibble.
        data <- assays(object)[[assay]] %>%
            # Ensure sparse matrix is coerced to dense.
            as.matrix() %>%
            as_tibble(rownames = "rowname") %>%
            gather(
                key = "colname",
                value = "counts",
                -UQ(sym("rowname"))
            )

        # SingleCellExperiment requires cell2sample mapping.
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

        # Prepare the minimal tibble required for plotting.
        data <- data %>%
            # Drop zero counts, which is useful when log scaling the axis.
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
            select(!!!syms(c("counts", "interestingGroups", biotypeCol)))

        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("interestingGroups"),
                y = !!sym("counts")
            )
        ) +
            geom_violin(
                mapping = aes(fill = !!sym("interestingGroups")),
                color = "black",
                scale = "area",
                trim = TRUE
            ) +
            scale_y_continuous(
                breaks = breaks,
                labels = prettyNum,
                trans = trans
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

        p
    }



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBiotype",
    signature = signature("SummarizedExperiment"),
    definition = plotCountsPerBiotype.SummarizedExperiment
)
