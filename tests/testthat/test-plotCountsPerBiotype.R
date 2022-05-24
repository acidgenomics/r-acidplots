test_that("RSE, SCE", {
    transes <- eval(formals(`plotCountsPerBiotype,SE`)[["trans"]])
    for (object in list(rse, sce)) {
        x <- plotCountsPerBiotype(object)
        expect_s3_class(x, "ggplot")
        for (trans in transes) {
            x <- plotCountsPerBiotype(object, trans = trans)
            expect_s3_class(x, "ggplot")
            x <- plotCountsPerBroadClass(object, trans = trans)
            expect_s3_class(x, "ggplot")
        }
    }
})
