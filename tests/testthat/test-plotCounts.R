context("plotCounts")

test_that("default", {
    x <- plotCounts(object = object, genes = genes)
    expect_s3_class(x, "ggplot")
    expect_identical(
        object = x[["labels"]],
        expected = list(
            x = NULL,
            y = "counts",
            colour = "condition",
            title = NULL,
            subtitle = NULL
        )
    )
    expect_s3_class(
        object = x[["layers"]][[1L]][["geom"]],
        class = "GeomPoint"
    )
    expect_s3_class(
        object = x[["layers"]][[2L]][["geom"]],
        class = "GeomCrossbar"
    )
})

with_parameters_test_that(
    "Gene IDs or names", {
        x <- plotCounts(object = object, genes = genes)
        expect_s3_class(x, "ggplot")
    },
    genes = list(
        geneIDs,
        geneNames
    )
)

trans <- eval(formals(`plotCounts,SummarizedExperiment`)[["trans"]])

with_parameters_test_that(
    "trans", {
        x <- plotCounts(object, genes = genes, trans = trans)
        expect_s3_class(x, "ggplot")
        expect_identical(
            object = x[["labels"]][["y"]],
            expected = ylab
        )
    },
    trans = trans,
    ylab = c(
        identity = "counts",
        log2 = "log2 counts",
        log10 = "log10 counts"
    )
)

style <- eval(formals(`plotCounts,SummarizedExperiment`)[["style"]])

with_parameters_test_that(
    "style", {
        x <- plotCounts(object, genes = genes, style = style)
        expect_s3_class(x, "ggplot")
        expect_is(x[["facet"]], facetClass)
    },
    style = style,
    facetClass = c(
        facet = "FacetWrap",
        wide = "FacetNull"
    )
)
