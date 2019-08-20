context("plotCountsPerCell")

object <- calculateMetrics(sce)

test_that("SingleCellExperiment", {
    p <- plotCountsPerCell(object)
    expect_s3_class(p, "ggplot")
})

skip_if_not_installed(pkg = "DropletUtils", minimum_version = "1.4")

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
