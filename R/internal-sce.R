#' Bind all dimension reduction matrices into a single data frame
#'
#' @note Updated 2022-05-24.
#' @noRd
.bindReducedDims <- function(object) {
    object <- as(object, "SingleCellExperiment")
    reducedDims <- reducedDims(object)
    assert(hasNames(reducedDims))
    ## Handle undefined column names here, which is currently the case with
    ## monocle3 UMAP (but not PCA) output.
    if (!isTRUE(all(bapply(reducedDims, hasColnames)))) {
        list <- Map(
            name = names(reducedDims),
            assay = reducedDims,
            f = function(name, assay) {
                if (!hasColnames(assay)) {
                    colnames(assay) <- paste0(name, seq_len(ncol(assay)))
                }
                assay
            }
        )
        reducedDims <- as(list, "SimpleList")
    }
    out <- do.call(what = cbind, args = reducedDims)
    out <- as(out, "DFrame")
    assert(
        hasValidDimnames(out),
        identical(x = colnames(object), y = rownames(out))
    )
    out
}



#' Fetch gene data
#'
#' @note Updated 2023-12-04.
#' @noRd
.fetchGeneData <-
    function(object,
             genes,
             assay = c("logcounts", "normcounts"),
             metadata = FALSE) {
        object <- as(object, "SingleCellExperiment")
        assert(
            isCharacter(genes),
            isFlag(metadata)
        )
        assay <- match.arg(assay)
        rownames <- mapGenesToRownames(object = object, genes = genes)
        assert(isSubset(rownames, rownames(object)))
        fun <- get(assay, inherits = TRUE)
        assert(is.function(fun))
        counts <- fun(object)
        counts <- counts[rownames, , drop = FALSE]
        ## Early return the transposed matrix, if we don't want metadata.
        ## This return is used by `.fetchReductionExpressionData()`.
        if (isFALSE(metadata)) {
            ## Transpose, putting genes into the columns.
            out <- t(counts)
            assert(identical(class(counts), class(out)))
            return(out)
        }
        ## Melt counts to long format data frame.
        data <- melt(
            object = counts,
            colnames = c("rowname", "colname", assay)
        )
        ## Join cell-level metrics. Always include `ident` and `sampleName`
        ## using `metrics` here. This ensures `sampleName` and
        ## `interestingGroups` are always defined.
        colData <- metrics(object)
        colData[["colname"]] <- as.factor(rownames(colData))
        data <- leftJoin(data, colData, by = "colname")
        ## Join the `geneId` and `geneName` columns by the `rowname` column.
        g2s <- GeneToSymbol(object, format = "makeUnique")
        assert(hasLength(g2s), hasRownames(g2s))
        g2s <- as(g2s, "DFrame")
        g2s[["rowname"]] <- as.factor(rownames(g2s))
        data <- leftJoin(data, g2s, by = "rowname")
        data <- mutateIf(data, is.character, as.factor)
        data <- data[, unique(c("rowname", sort(colnames(data))))]
        colnames(data) <- camelCase(colnames(data), strict = TRUE)
        data
    }



#' Fetch reduction data
#'
#' @note Updated 2021-03-03.
#' @noRd
.fetchReductionData <-
    function(object,
             reduction = 1L,
             dims = seq_len(2L)) {
        object <- as(object, "SingleCellExperiment")
        assert(
            hasClusters(object),
            isScalar(reduction),
            hasLength(dims, n = 2L),
            all(isIntegerish(dims))
        )
        if (isString(reduction)) {
            reduction <- camelCase(reduction, strict = TRUE)
            assert(isCharacter(reducedDimNames(object)))
            reducedDimNames(object) <-
                camelCase(reducedDimNames(object), strict = TRUE)
        }
        redData <- reducedDim(object, type = reduction)
        assert(hasLength(redData))
        if (!hasColnames(redData)) {
            colnames(redData) <- paste0(reduction, seq_len(ncol(redData)))
        }
        redData <- as(redData, "DFrame")
        ## Cellular barcode metrics.
        colData <- metrics(object)
        assert(
            isSubset("ident", colnames(colData)),
            identical(
                x = rownames(redData),
                y = rownames(colData)
            ),
            areDisjointSets(
                x = colnames(redData),
                y = colnames(colData)
            )
        )
        dimCols <- colnames(redData)[dims]
        assert(is.character(dimCols))
        ## Bind the data frames.
        data <- cbind(redData, colData)
        assert(is(data, "DFrame"))
        ## Split by cluster.
        f <- data[["ident"]]
        assert(
            is.factor(f),
            !all(is.na(f))
        )
        split <- split(x = data, f = f)
        assert(is(split, "SplitDFrameList"))
        split <- SplitDataFrameList(lapply(
            X = split,
            FUN = function(x) {
                x[["x"]] <- x[[dimCols[[1L]]]]
                x[["y"]] <- x[[dimCols[[2L]]]]
                x[["centerX"]] <- median(x[["x"]])
                x[["centerY"]] <- median(x[["y"]])
                x
            }
        ))
        data <- unsplit(split, f = f)
        assert(
            hasRownames(data),
            areSetEqual(rownames(data), colnames(object))
        )
        data <- data[colnames(object), , drop = FALSE]
        colnames(data) <- camelCase(colnames(data), strict = TRUE)
        data
    }

formals(.fetchReductionData)[c("dims", "reduction")] <-
    .formalsList[c("dims", "reduction")]



## Updated 2021-03-03.
.fetchReductionExpressionData <-
    function(object,
             genes,
             reduction,
             assay = "logcounts") {
        object <- as(object, "SingleCellExperiment")
        assert(
            is.character(genes),
            isScalar(reduction),
            isString(assay)
        )
        rownames <- mapGenesToRownames(object, genes = genes)
        ## Transposed count matrix, with genes in the columns.
        geneCounts <- .fetchGeneData(
            object = object,
            genes = rownames,
            assay = assay,
            metadata = FALSE
        )
        assert(identical(
            x = colnames(geneCounts),
            y = as.character(rownames)
        ))
        if (is(geneCounts, "Matrix")) {
            assert(requireNamespaces("Matrix"))
            rowMeans <- Matrix::rowMeans
            rowSums <- Matrix::rowSums
        }
        ## Note that `rowMedians` currently isn't supported for sparse data.
        mean <- rowMeans(geneCounts)
        sum <- rowSums(geneCounts)
        ## Fetch reduced dim data.
        reductionData <- .fetchReductionData(
            object = object,
            reduction = reduction
        )
        data <- cbind(reductionData, mean, sum)
        assert(is(data, "DFrame"))
        colnames(data) <- camelCase(colnames(data), strict = TRUE)
        data
    }

formals(.fetchReductionExpressionData)[["reduction"]] <-
    .formalsList[["reduction"]]



#' Generate a grouping factor
#'
#' Use this internal function to define grouping factor for `split()` call.
#'
#' Use `[` subsetting to request specific columns.
#'
#' @noRd
#' @note Updated 2019-09-01.
.group <- function(x) {
    f <- apply(
        X = as.data.frame(x),
        MARGIN = 1L,
        FUN = paste,
        collapse = "."
    )
    f <- make.names(f, unique = FALSE)
    f <- as.factor(f)
    f
}
