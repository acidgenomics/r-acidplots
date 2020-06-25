context("plotWaterfall")

test_that("data.frame", {
    object <- data.frame(
        cell_id = paste("cell", seq_len(12L), sep = "_"),
        ic50 = seq(
        from = 0.1,
            to = 10L,
            length.out = 12L
        ),
        tumor_type = rep(
            x = c("breast", "bladder"),
            times = 6L
        ),
        tumor_subtype = rep(
            x = c("benign", "malignant"),
            each = 6L
        )
    )
    p <- plotWaterfall(
        object = object,
        sampleCol = "cell_id",
        valueCol = "ic50",
        interestingGroups = c("tumor_type", "tumor_subtype"),
        labels = list(
            title = "Effect of compound on cell survival"
        )
    )
    expect_s3_class(p, "ggplot")
})
