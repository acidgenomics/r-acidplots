## Updated 2019-08-21.
.emptyPheatmapAnnotations <- list(
    annotationCol = NA,
    annotationColors = NA
)



## Automatically handle the annotation data and colors.
## Factors with a single level are automatically dropped.
## Updated 2019-08-21.
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
    ## Prepare the annotation columns.
    data <- colData(object)
    interestingGroups <- interestingGroups(object)
    ## pheatmap requires `NA` if annotations are empty.
    if (
        !hasDims(data) ||
        !hasLength(interestingGroups) ||
        identical(interestingGroups, "sampleName")
    ) {
        return(.emptyPheatmapAnnotations)
    }
    assert(
        hasRownames(data),
        isSubset(interestingGroups, colnames(data))
    )
    data <- data[, interestingGroups, drop = FALSE]
    ## Remove blacklisted columns (e.g. `sampleName`).
    blacklist <- unique(c("sampleName", blacklist))
    data <- data[, setdiff(colnames(data), blacklist), drop = FALSE]
    ## Select only factor columns.
    keep <- which(bapply(data, is.factor))
    data <- data[, keep, drop = FALSE]
    ## Drop any remaining factor columns that contain a single value. Note that
    ## we don't want to necessarily use `levels()` in place of `unique()` here,
    ## in case we have a situation where we're comparing a value against `NA`.
    ## Here this will a level of 1, even though we have 2 unique values.
    hasMultiple <- vapply(
        X = data,
        FUN = function(x) {
            ## This handles NA values better than using `levels()`.
            length(unique(x)) > 1L
        },
        FUN.VALUE = logical(1L)
    )
    ## Return empty if there are no useful factor columns.
    if (!hasLength(hasMultiple)) {
        return(.emptyPheatmapAnnotations)  # nocov
    } else {
        data <- data[, hasMultiple, drop = FALSE]
    }
    ## Automatically define colors.
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
    ## Return.
    list(
        annotationCol = as.data.frame(data),
        annotationColors = colors
    )
}



## Sanitize formals into snake case and abort on duplicates. Duplicates may
## arise if user is mixing and matching camel/snake case.
## Updated 2019-08-21.
.pheatmapArgs <- function(args) {
    assert(is.list(args), hasNames(args))
    ## Abort on snake case formatted formal args.
    invalidNames <- grep("[._]", names(args), value = TRUE)
    if (length(invalidNames) > 0L) {
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



## If `color = NULL`, use the pheatmap default palette.
## Updated 2019-08-21.
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
