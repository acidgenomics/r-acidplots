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

geoms <- eval(formals(`plotQC,SCE`)[["geom"]])
test_that("geom", {
    for (geom in geoms) {
        p <- plotQC(sce, geom = geom)
        expect_s3_class(p, "ggplot")
    }
})
