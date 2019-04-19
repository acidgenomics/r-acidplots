#' @name plotCountsPerBroadClass
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit bioverbs::plotCountsPerBroadClass
#' @inheritParams params
#' @examples
#' data(rse, sce, package = "acidtest")
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
#' @export
NULL



plotCountsPerBroadClass.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        trans = c("log10", "log2", "identity"),
        countsAxisLabel = "counts",
        title = "Counts per broad class"
    ) {
        validObject(object)
        assert(
            isScalar(assay),
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
        rownames(rowData) <- rownames(object)

        biotypeCol <- "broadClass"
        # Warn and early return if the biotypes are not defined in rowData.
        if (!biotypeCol %in% colnames(rowData)) {
            warning(paste(
                "rowData() does not contain biotypes defined in",
                biotypeCol, "column."
            ))
            return(invisible())
        }

        biotypes <- rowData %>%
            as_tibble() %>%
            select(!!sym(biotypeCol)) %>%
            group_by(!!sym(biotypeCol)) %>%
            summarise(n = n()) %>%
            # Require at least 10 genes.
            filter(!!sym("n") >= 10L) %>%
            arrange(desc(!!sym("n"))) %>%
            pull(!!sym(biotypeCol)) %>%
            as.character()

        # Coerce the sample data to a tibble.
        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID") %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID")))

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

        data <- data %>%
            filter(!!sym("counts") > 0L) %>%
            left_join(
                as_tibble(rowData, rownames = "rowname"),
                by = "rowname"
            ) %>%
            filter(!!sym(biotypeCol) %in% !!biotypes) %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID"))) %>%
            left_join(
                y = as_tibble(sampleData),
                by = "sampleID"
            )

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
    f = "plotCountsPerBroadClass",
    signature = signature("SummarizedExperiment"),
    definition = plotCountsPerBroadClass.SummarizedExperiment
)
