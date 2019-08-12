context("plotCountsPerCell")

object <- calculateMetrics(sce)

test_that("SingleCellExperiment", {
    p <- plotCountsPerCell(object)
    expect_s3_class(p, "ggplot")
})

point <- eval(formals(`plotCountsPerCell,SingleCellExperiment`)[["point"]])
with_parameters_test_that(
    "Inflection/knee point labeling", {
        p <- plotCountsPerCell(
            object = object,
            point = point,
            geom = "ecdf"
        )
        expect_s3_class(p, "ggplot")
    },
    point = point
)
