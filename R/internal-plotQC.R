#' Match sample identifier column
#'
#' @note Updated 2021-01-15.
#' @note Consider adding this into basejump in a future update.
#' @noRd
#'
#' @details
#' Supports (in order of preference):
#' - `sampleId` (preferred).
#' - `sampleID` (legacy).
#' - `sampleid` (unused).
#' - `sample` (last resort).
#'
#' @return `character(1)` or error on match failure.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#' object <- SingleCellExperiment
#' id <- .matchSampleIdCol(object)
.matchSampleIdCol <- function(object) {
    assert(is(object, "SummarizedExperiment"))
    x <- colnames(colData(object))
    table <- c("sampleId", "sampleID", "sampleid", "sample")
    match <- match(x = x, table = table)
    if (all(is.na(match))) {
        stop(sprintf(
            paste0(
                "Failed to match sample identifier in '%s()'.\n",
                "Expecting (in order of preference): %s."
            ),
            "colData", toString(table)
        ))
    }
    id <- table[min(na.omit(match))]
    id
}



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
        stop(sprintf("'%s' is not defined in 'colData()'.", metricCol))
    } else if (anyNA(data[[metricCol]])) {
        stop(sprintf("'%s' in 'colData()' contains NA values.", metricCol))
    } else if (all(data[[metricCol]] == 0L)) {
        stop(sprintf("'%s' in 'colData()' contains only zeros.", metricCol))
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
        metricAxis <- "x"
        p <- p +
            geom_density_ridges(
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

f <- formals(`.plotQCMetric`)
f[c("color", "fill")] <- formalsList[c("color.discrete", "fill.discrete")]
f[["geom"]] <- .geom
formals(`.plotQCMetric`) <- f



## Compare two quality control metrics.
## Updated 2019-09-15.
.plotQCScatterplot <- function(
    object,
    xCol,
    yCol,
    xTrans = "identity",
    yTrans = "identity",
    interestingGroups = NULL,
    trendline = FALSE,
    color = getOption("basejump.discrete.color", NULL),
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
        stop(sprintf(
            "%s are not defined in 'colData()'.",
            toString(c(xCol, yCol))
        ))
    } else if (anyNA(data[[xCol]])) {
        stop(sprintf("'%s' in 'colData()' contains NA values.", xCol))
    } else if (anyNA(data[[yCol]])) {
        stop(sprintf("'%s' in 'colData()' contains NA values.", yCol))
    } else if (all(data[[xCol]] == 0L)) {
        stop(sprintf("'%s' in 'colData()' contains only zeros.", xCol))
    } else if (all(data[[yCol]] == 0L)) {
        stop(sprintf("'%s' in 'colData()' contains only zeros.", yCol))
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
