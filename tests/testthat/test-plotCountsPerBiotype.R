context("plotCountsPerBiotype")

with_parameters_test_that(
    "RSE, SCE", {
        x <- plotCountsPerBiotype(object)
        expect_s3_class(x, "ggplot")
    },
    object = list(rse, sce)
)

trans <- eval(formals(`plotCountsPerBiotype,SummarizedExperiment`)[["trans"]])
with_parameters_test_that(
    "trans", {
        x <- plotCountsPerBiotype(rse, trans = trans)
        expect_s3_class(x, "ggplot")
    },
    trans = trans
)



context("plotCountsPerBroadClass")

with_parameters_test_that(
    "RSE, SCE", {
        x <- plotCountsPerBroadClass(object)
        expect_s3_class(x, "ggplot")
    },
    object = list(rse, sce)
)

trans <-
    eval(formals(`plotCountsPerBroadClass,SummarizedExperiment`)[["trans"]])
with_parameters_test_that(
    "trans", {
        x <- plotCountsPerBroadClass(rse, trans = trans)
        expect_s3_class(x, "ggplot")
    },
    trans = trans
)
