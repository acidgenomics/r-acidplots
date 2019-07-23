context("ggplot2 themes")

with_parameters_test_that(
    "themes", {
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
    },
    theme = list(
        acid_theme_light,
        acid_theme_dark
    )
)
