#' Empty pheatmap annotations
#'
#' @note Updated 2019-08-21.
#' @noRd
.emptyPheatmapAnnotations <- list(
    annotationCol = NA,
    annotationColors = NA
)



#' Generate pheatmap annotation data
#'
## Automatically handle the annotation data and colors.
#'
#' @note Updated 2020-08-25.
#' @noRd
#'
#' Factors with a single level are automatically dropped.
#'
#' pheatmap requires `NA` if annotations are empty.
#'
#' Drop any remaining factor columns that contain a single value. Note that
#' we don't want to necessarily use `levels()` in place of `unique()` here,
#' in case we have a situation where we're comparing a value against `NA`.
#' Here this will a level of 1, even though we have 2 unique values. This
#' approach handles NA values better than using `levels()`.
.pheatmapAnnotations <- function(
    object,
    blacklist = "sampleName",
    legendColor
) {
    assert(
        is(object, "SummarizedExperiment"),
        isCharacter(blacklist),
        isHexColorFunction(legendColor, nullOK = TRUE)
    )
    empty <- .emptyPheatmapAnnotations
    data <- colData(object)
    interestingGroups <- interestingGroups(object)
    ok <- hasDims(data) &&
        hasLength(interestingGroups) &&
        !identical(interestingGroups, "sampleName")
    if (!isTRUE(ok)) return(empty)
    assert(
        hasRownames(data),
        isSubset(interestingGroups, colnames(data))
    )
    data <- data[, interestingGroups, drop = FALSE]
    blacklist <- unique(c("sampleName", blacklist))
    cols <- setdiff(colnames(data), blacklist)
    if (!hasLength(cols)) return(empty)
    data <- data[, cols, drop = FALSE]
    keep <- bapply(X = data, FUN = is.factor)
    if (!any(keep)) return(empty)
    data <- data[, keep, drop = FALSE]
    rownames <- rownames(data)
    data <- as.data.frame(lapply(
        X = data,
        FUN = function(x) {
            x <- as.character(x)
            x <- str_replace_na(x)
            x <- as.factor(x)
            x
        }
    ))
    rownames(data) <- rownames
    hasMultiple <- bapply(
        X = data,
        FUN = function(x) {
            length(unique(x)) > 1L
        }
    )
    if (!hasLength(hasMultiple)) return(empty)
    data <- data[, hasMultiple, drop = FALSE]
    if (is.function(legendColor)) {
        colors <- lapply(
            X = data,
            FUN = function(x) {
                assert(is.factor(x))
                levels <- levels(x)
                colors <- legendColor(length(levels))
                names(colors) <- levels
                colors
            })
        names(colors) <- colnames(data)
    } else {
        colors <- NA
    }
    list(
        annotationCol = as.data.frame(data),
        annotationColors = colors
    )
}



#' Generate pheatmap arguments
#'
#' @note Updated 2019-08-21.
#' @noRd
#'
#' Sanitize formals into snake case and abort on duplicates. Duplicates may
#' arise if user is mixing and matching camel/snake case.
.pheatmapArgs <- function(args) {
    assert(is.list(args), hasNames(args))
    ## Abort on snake case formatted formal args.
    invalidNames <- grep("[._]", names(args), value = TRUE)
    if (hasLength(invalidNames)) {
        stop(sprintf(
            "Specify arguments in camel case: %s.",
            toString(invalidNames, width = 100L)
        ))
    }
    names(args) <- snakeCase(names(args))
    assert(
        isSubset(names(args), formalArgs(pheatmap)),
        hasNoDuplicates(names(args))
    )
    args
}



#' Generate pheatmap color palette
#'
#' @note Updated 2019-08-21.
#' @noRd
#'
#' If `color = NULL`, use the pheatmap default palette.
.pheatmapColorPalette <- function(color = NULL, n = 256L) {
    if (is.character(color)) {
        ## Hexadecimal color palette (e.g. RColorBrewer, viridis return).
        assert(allAreHexColors(color))
        color
    } else if (is.function(color)) {
        ## Hexadecimal color function (e.g. viridis functions).
        assert(isHexColorFunction(color))
        color(n = n)
    } else {
        ## pheatmap default palette.
        ## Note that `n` argument won't get evaluated here.
        eval(formals(pheatmap)[["color"]])
    }
}
