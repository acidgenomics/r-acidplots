test_that("themes", {
    for (theme in list(
        acid_theme_light,
        acid_theme_dark
    )) {
        p <- ggplot(
            data = mpg,
            mapping = aes(
                x = manufacturer,
                y = displ,
                color = manufacturer,
                fill = manufacturer
            )
        ) +
            geom_point() +
            theme()
        expect_s3_class(p, "ggplot")
    }
})
