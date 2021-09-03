#' Plot a single quality control metric
#'
#' @note Updated 2019-12-09.
#' @noRd
.plotQCMetric <- function(
    object,
    metricCol,
    geom,
    interestingGroups = NULL,
    min = 0L,
    max = Inf,
    trans = "identity",
    ratio = FALSE,
    color,
    fill,
    labels = list(
        title = NULL,
        subtitle = NULL,
        metricAxis = NULL,
        otherAxis = NULL
    )
) {
    validObject(object)
    assert(
        is(object, "SingleCellExperiment"),
        isString(metricCol),
        all(isNonNegative(c(min, max))),
        isString(trans),
        isGGScale(fill, scale = "discrete", aes = "fill", nullOK = TRUE),
        is.list(labels),
        areSetEqual(
            x = names(labels),
            y = names(eval(formals()[["labels"]]))
        )
    )
    geom <- match.arg(geom)
    if (is.null(labels[["metricAxis"]])) {
        labels[["metricAxis"]] <- makeLabel(metricCol)
    }
    interestingGroups(object) <-
        matchInterestingGroups(object, interestingGroups)
    ## Support for per sample filtering cutoffs.
    min <- min(min)
    max <- max(max)
    if (isTRUE(ratio)) {
        assert(all(isInRange(c(min, max), lower = 0L, upper = 1L)))
    }
    data <- metrics(object)
    data <- as_tibble(data, rownames = NULL)
    ## nocov start
    if (!isSubset(metricCol, colnames(data))) {
        abort(sprintf(
            "{.var %s} is not defined in {.fun %s}.",
            metricCol, "colData"
        ))
    } else if (anyNA(data[[metricCol]])) {
        abort(sprintf(
            "{.var %s} in {.fun %s} contains NA values.",
            metricCol, "colData"
        ))
    } else if (all(data[[metricCol]] == 0L)) {
        abort(sprintf(
            "{.var %s} in {.fun %s} contains only zeros.",
            metricCol, "colData"
        ))
    }
    ## nocov end
    mapping <- aes(
        color = str_replace_na(!!sym("interestingGroups")),
        fill = str_replace_na(!!sym("interestingGroups"))
    )
    if (isSubset(geom, c("boxplot", "violin"))) {
        mapping[["x"]] <- as.symbol("sampleName")
        mapping[["y"]] <- as.symbol(metricCol)
    } else if (identical(geom, "ridgeline")) {
        ## Ridgeline flips the axes.
        mapping[["x"]] <- as.symbol(metricCol)
        mapping[["y"]] <- as.symbol("sampleName")
    } else if (isSubset(geom, c("ecdf", "histogram"))) {
        mapping[["x"]] <- as.symbol(metricCol)
    }
    p <- ggplot(data = data, mapping = mapping)
    metricAxis <- "y"
    if (identical(geom, "boxplot")) {
        p <- p +
            geom_boxplot(color = "black", outlier.shape = NA) +
            scale_y_continuous(trans = trans)
    } else if (identical(geom, "ecdf")) {
        metricAxis <- "x"
        p <- p +
            stat_ecdf(geom = "step", size = 1L) +
            scale_x_continuous(trans = trans)
        labels[["otherAxis"]] <- "Fn(x)"
    } else if (identical(geom, "histogram")) {
        metricAxis <- "x"
        p <- p +
            geom_histogram(
                bins = 200L,
                color = FALSE
            ) +
            scale_x_continuous(trans = trans) +
            scale_y_continuous()
        labels[["otherAxis"]] <- "count"
    } else if (identical(geom, "ridgeline")) {
        requireNamespaces("ggridges")
        metricAxis <- "x"
        p <- p +
            ggridges::geom_density_ridges(
                alpha = 0.8,
                color = "black",
                panel_scaling = TRUE,
                scale = 10L
            ) +
            scale_x_continuous(trans = trans)
    } else if (identical(geom, "violin")) {
        p <- p +
            geom_violin(
                color = "black",
                scale = "area",
                trim = TRUE
            ) +
            scale_y_continuous(trans = trans)
    }
    ## Cutoff lines.
    if (isSubset(geom, c("boxplot", "violin"))) {
        if (isTRUE(min > 0L)) {
            p <- p + acid_geom_abline(yintercept = min)
        }
        if (
            (isTRUE(max < Inf) && isFALSE(ratio)) ||
            (isTRUE(max < 1L) && isTRUE(ratio))
        ) {
            p <- p + acid_geom_abline(yintercept = max)
        }
    } else {
        if (isTRUE(min > 0L)) {
            p <- p + acid_geom_abline(xintercept = min)
        }
        if (
            (isTRUE(max < Inf) && identical(ratio, FALSE)) ||
            (isTRUE(max < 1L) && identical(ratio, TRUE))
        ) {
            p <- p + acid_geom_abline(xintercept = max)
        }
    }
    ## Labels.
    if (is.list(labels)) {
        names(labels)[names(labels) == "metricAxis"] <- metricAxis
        otherAxis <- ifelse(
            test = identical(metricAxis, "y"),
            yes = "x",
            no = "y"
        )
        names(labels)[names(labels) == "otherAxis"] <- otherAxis
        labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
        labels[["fill"]] <- labels[["color"]]
        p <- p + do.call(what = labs, args = labels)
    }
    ## Color palette.
    if (identical(geom, "ecdf")) {
        if (is(color, "ScaleDiscrete")) {
            p <- p + color
        }
    } else {
        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }
    }
    ## Median labels.
    if (!isSubset(geom, c("ecdf", "histogram"))) {
        if (isSubset(metricCol, c("log10GenesPerUMI", "mitoRatio"))) {
            digits <- 2L
        } else {
            digits <- 0L
        }
        p <- p +
            acid_geom_label_average(
                data = data,
                col = metricCol,
                digits = digits
            )
    }
    ## Facets.
    facets <- NULL
    if (isSubset("aggregate", colnames(data))) {
        facets <- "aggregate"
    }
    if (is.character(facets)) {
        p <- p + facet_wrap(facets = syms(facets), scales = "free")
    }
    ## Return.
    p
}

formals(`.plotQCMetric`)[c("color", "fill", "geom")] <-
    list(
        "color" = formalsList[["color.discrete"]],
        "fill" = formalsList[["fill.discrete"]],
        "geom" = .formalsList[["geom"]]
    )



## Compare two quality control metrics.
## Updated 2021-02-08.
.plotQCScatterplot <- function(
    object,
    xCol,
    yCol,
    xTrans = "identity",
    yTrans = "identity",
    interestingGroups = NULL,
    trendline = FALSE,
    color = getOption("acid.discrete.color", NULL),
    labels = list(
        title = NULL,
        subtitle = NULL,
        x = NULL,
        y = NULL
    )
) {
    validObject(object)
    assert(
        is(object, "SingleCellExperiment"),
        isString(xCol),
        isString(yCol),
        isString(xTrans),
        isString(yTrans),
        isGGScale(color, scale = "discrete", aes = "color", nullOK = TRUE),
        is.list(labels),
        areSetEqual(
            x = names(labels),
            y = names(eval(formals()[["labels"]]))
        )
    )
    ## Generate x- and y-axis labels automatically.
    if (is.null(labels[["x"]])) {
        labels[["x"]] <- makeLabel(xCol)
    }
    if (is.null(labels[["y"]])) {
        labels[["y"]] <- makeLabel(yCol)
    }
    interestingGroups(object) <-
        matchInterestingGroups(object, interestingGroups)
    data <- metrics(object)
    data <- as_tibble(data, rownames = NULL)
    ## nocov start
    if (!isSubset(c(xCol, yCol), colnames(data))) {
        abort(sprintf(
            "Not defined in {.fun %s}: %s.",
            "colData", toInlineString(c(xCol, yCol))
        ))
    } else if (anyNA(data[[xCol]])) {
        abort(sprintf(
            "{.var %s} in {.fun %s} contains NA values.",
            xCol, "colData"
        ))
    } else if (anyNA(data[[yCol]])) {
        abort(sprintf(
            "{.var %s} in {.fun %s} contains NA values.",
            yCol, "colData"
        ))
    } else if (all(data[[xCol]] == 0L)) {
        abort(sprintf(
            "{.var %s} in {.fun %s} contains only zeros.",
            xCol, "colData"
        ))
    } else if (all(data[[yCol]] == 0L)) {
        abort(sprintf(
            "{.var %s} in {.fun %s} contains only zeros.",
            yCol, "colData"
        ))
    }
    ## nocov end
    p <- ggplot(
        data = data,
        mapping = aes(
            x = !!sym(xCol),
            y = !!sym(yCol),
            color = str_replace_na(!!sym("interestingGroups"))
        )
    ) +
        geom_point(alpha = 0.5, size = 1L) +
        scale_x_continuous(trans = xTrans) +
        scale_y_continuous(trans = yTrans)
    ## Trendline.
    if (isTRUE(trendline)) {
        ## If `method = "gam"`, mgcv package is required.
        ## Otherwise build checks will error.
        p <- p + geom_smooth(method = "glm", se = FALSE, size = 1L)
    }
    ## Labels.
    if (is.list(labels)) {
        labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
        p <- p + do.call(what = labs, args = labels)
    }
    ## Color palette.
    if (is(color, "ScaleDiscrete")) {
        p <- p + color
    }
    ## Facets.
    facets <- NULL
    if (isSubset("aggregate", colnames(data))) {
        facets <- c(facets, "aggregate")
    }
    if (is.character(facets)) {
        p <- p + facet_wrap(facets = syms(facets), scales = "free")
    }
    ## Return.
    p
}
