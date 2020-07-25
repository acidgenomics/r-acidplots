context("plotCounts")

test_that("default", {
    x <- plotCounts(
        object = object,
        genes = genes,
        line = "median"
    )
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

test_that("Gene IDs or names", {
    for (genes in list(geneIDs, geneNames)) {
        x <- plotCounts(object = object, genes = genes)
        expect_s3_class(x, "ggplot")
    }
})

trans <- eval(formals(`plotCounts,SummarizedExperiment`)[["trans"]])
test_that("trans", {
    mapply(
        trans = trans,
        ylab = c(
            identity = "counts",
            log2 = "log2 counts",
            log10 = "log10 counts"
        ),
        FUN = function(trans, ylab) {
            x <- plotCounts(object, genes = genes, trans = trans)
            expect_s3_class(x, "ggplot")
            expect_identical(
                object = x[["labels"]][["y"]],
                expected = ylab
            )
        },
        SIMPLIFY = FALSE
    )
})

style <- eval(formals(`plotCounts,SummarizedExperiment`)[["style"]])
test_that("style", {
    mapply(
        style = style,
        facetClass = c(
            facet = "FacetWrap",
            wide = "FacetNull"
        ),
        FUN = function(style, facetClass) {
            x <- plotCounts(object, genes = genes, style = style)
            expect_s3_class(x, "ggplot")
            expect_is(x[["facet"]], facetClass)
        },
        SIMPLIFY = FALSE
    )
})
