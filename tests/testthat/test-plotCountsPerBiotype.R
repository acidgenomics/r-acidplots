context("plotCountsPerBiotype")

test_that("RSE, SCE", {
    for (object in list(rse, sce)) {
        x <- plotCountsPerBiotype(object)
        expect_s3_class(x, "ggplot")
    }
})

transes <- eval(formals(`plotCountsPerBiotype,SE`)[["trans"]])
test_that("trans", {
    for (trans in transes) {
        x <- plotCountsPerBiotype(rse, trans = trans)
        expect_s3_class(x, "ggplot")
    }
})



context("plotCountsPerBroadClass")

test_that("RSE, SCE", {
    for (object in list(rse, sce)) {
        x <- plotCountsPerBroadClass(object)
        expect_s3_class(x, "ggplot")
    }
})

transes <-
    eval(formals(`plotCountsPerBroadClass,SE`)[["trans"]])
test_that("trans", {
    for (trans in transes) {
        x <- plotCountsPerBroadClass(rse, trans = trans)
        expect_s3_class(x, "ggplot")
    }
})
