#' Plot a single quality control metric
#'
#' @note Updated 2022-11-09.
#' @noRd
.plotQCMetric <-
    function(object,
             assay = 1L,
             metricCol,
             geom,
             interestingGroups = NULL,
             min = 0L,
             max = Inf,
             trans = "identity",
             ratio = FALSE,
             labels = list(
                 "title" = NULL,
                 "subtitle" = NULL,
                 "metricAxis" = NULL,
                 "otherAxis" = NULL
             )) {
        validObject(object)
        assert(
            is(object, "SingleCellExperiment"),
            isString(metricCol),
            all(isNonNegative(c(min, max))),
            isString(trans)
        )
        geom <- match.arg(geom)
        labels <- matchLabels(labels)
        if (is.null(labels[["metricAxis"]])) {
            labels[["metricAxis"]] <- makeLabel(metricCol)
        }
        interestingGroups(object) <-
            matchInterestingGroups(object, interestingGroups)
        interestingGroups <- interestingGroups(object)
        if (!hasMetrics(object)) {
            suppressMessages({
                object <- calculateMetrics(object, assay = assay)
            })
        }
        ## Support for per sample filtering cutoffs.
        min <- min(min)
        max <- max(max)
        if (isTRUE(ratio)) {
            assert(all(isInRange(c(min, max), lower = 0L, upper = 1L)))
        }
        data <- metrics(object)
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
            color = str_replace_na(.data[["interestingGroups"]]),
            fill = str_replace_na(.data[["interestingGroups"]])
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
        p <- ggplot(data = as.data.frame(data), mapping = mapping)
        metricAxis <- "y"
        if (identical(geom, "boxplot")) {
            p <- p +
                geom_boxplot(color = "black", outlier.shape = NA) +
                scale_y_continuous(trans = trans)
        } else if (identical(geom, "ecdf")) {
            metricAxis <- "x"
            p <- p +
                stat_ecdf(geom = "step", linewidth = 1L) +
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
        ## Color palette.
        if (identical(geom, "ecdf")) {
            p <- p + acid_scale_color_discrete()
        } else {
            p <- p + acid_scale_fill_discrete()
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
                    data = as.data.frame(data),
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
            p <- p + facet_wrap(
                facets = vars(!!!syms(facets)),
                scales = "free"
            )
        }
        ## Return.
        p
    }

formals(`.plotQCMetric`)[["geom"]] <-
    .formalsList[["geom"]]



#' Compare two quality control metrics
#'
#' @note Updated 2022-03-07.
#' @noRd
.plotQCScatterplot <-
    function(object,
             assay = 1L,
             xCol,
             yCol,
             xTrans = "identity",
             yTrans = "identity",
             interestingGroups = NULL,
             trendline = FALSE,
             labels = list(
                 "title" = NULL,
                 "subtitle" = NULL,
                 "x" = NULL,
                 "y" = NULL
             )) {
        validObject(object)
        assert(
            is(object, "SingleCellExperiment"),
            isString(xCol),
            isString(yCol),
            isString(xTrans),
            isString(yTrans)
        )
        labels <- matchLabels(labels)
        if (!hasMetrics(object)) {
            suppressMessages({
                object <- calculateMetrics(object, assay = assay)
            })
        }
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
        assert(
            isSubset(c(xCol, yCol), colnames(data)),
            msg = sprintf(
                "Not defined in {.fun %s}: %s.",
                "colData", toInlineString(c(xCol, yCol))
            )
        )
        assert(
            !anyNA(data[[xCol]]),
            msg = sprintf(
                "{.var %s} in {.fun %s} contains NA values.",
                xCol, "colData"
            )
        )
        assert(
            !anyNA(data[[yCol]]),
            msg = sprintf(
                "{.var %s} in {.fun %s} contains NA values.",
                yCol, "colData"
            )
        )
        assert(
            !all(data[[xCol]] == 0L),
            msg = sprintf(
                "{.var %s} in {.fun %s} contains only zeros.",
                xCol, "colData"
            )
        )
        assert(
            !all(data[[yCol]] == 0L),
            msg = sprintf(
                "{.var %s} in {.fun %s} contains only zeros.",
                yCol, "colData"
            )
        )
        p <- ggplot(
            data = as.data.frame(data),
            mapping = aes(
                x = .data[[xCol]],
                y = .data[[yCol]],
                color = str_replace_na(.data[["interestingGroups"]])
            )
        ) +
            geom_point(alpha = 0.5, size = 1L) +
            scale_x_continuous(trans = xTrans) +
            scale_y_continuous(trans = yTrans)
        ## Trendline.
        if (isTRUE(trendline)) {
            ## If `method = "gam"`, mgcv package is required.
            ## Otherwise build checks will error.
            p <- p + geom_smooth(linewidth = 1L, method = "glm", se = FALSE)
        }
        ## Labels.
        labels[["color"]] <- paste(interestingGroups, collapse = ":\n")
        p <- p + do.call(what = labs, args = labels)
        ## Color palette.
        p <- p + acid_scale_color_discrete()
        ## Facets.
        facets <- NULL
        if (isSubset("aggregate", colnames(data))) {
            facets <- c(facets, "aggregate")
        }
        if (is.character(facets)) {
            p <- p + facet_wrap(
                facets = vars(!!!syms(facets)),
                scales = "free"
            )
        }
        ## Return.
        p
    }
