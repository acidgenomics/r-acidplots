context("plotQC : SummarizedExperiment")

test_that("grid", {
    p <- plotQC(rse)
    expect_s3_class(p, "ggplot")
})



context("plotQC : SingleCellExperiment")

sce <- calculateMetrics(sce)

test_that("grid", {
    p <- plotQC(sce)
    expect_s3_class(p, "ggplot")
})

geom <- eval(formals(`plotQC,SingleCellExperiment`)[["geom"]])
with_parameters_test_that(
    "geom", {
        p <- plotQC(sce, geom = geom)
        expect_s3_class(p, "ggplot")
    },
    geom = geom
)
