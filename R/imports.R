## S4 classes ==================================================================

#' @importClassesFrom AcidSingleCell KnownMarkers
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics Gene2Symbol aggregateCellsToSamples
#'   as.SummarizedExperiment barcodeRanksPerSample camelCase cell2sample
#'   convertGenesToSymbols interestingGroups interestingGroups<-
#'   intersectionMatrix leftJoin makeLabel makeTitle mapGenesToRownames
#'   matchSampleColumn melt metrics mutateIf nonzeroRowsAndCols plotBarcodeRanks
#'   plotCellCounts plotCellCountsPerCluster plotCellTypesPerCluster
#'   plotCorrelation plotCorrelationHeatmap plotCounts plotCountsCorrelation
#'   plotCountsCorrelationHeatmap plotCountsPerBiotype plotCountsPerBroadClass
#'   plotCountsPerCell plotCountsPerFeature plotCountsVsFeatures plotDots
#'   plotFeature plotFeaturesDetected plotFeaturesPerCell plotGenderMarkers
#'   plotHeatmap plotKnownMarkers plotMarker plotMitoRatio plotNovelty plotQC
#'   plotQuantileHeatmap plotReducedDim plotStackedBarPlot plotSums plotTSNE
#'   plotTotalCounts plotUpset plotUMAP plotViolin plotWaterfall
#'   plotZerosVsDepth sampleData sem snakeCase zerosVsDepth
#' @importFrom Biobase sampleNames
#' @importFrom BiocGenerics append colSums counts do.call organism plotPCA rbind
#'   var
#' @importFrom IRanges quantile
#' @importFrom S4Vectors aggregate complete.cases decode head mcols merge
#'   metadata split
#' @importFrom SingleCellExperiment logcounts
#' @importFrom SummarizedExperiment assayNames assay assay<- assays assays<-
#'   colData rowData
#' @importFrom methods coerce
#'
#' @importMethodsFrom AcidBase intersectionMatrix sem
#' @importMethodsFrom AcidExperiment aggregate as.SummarizedExperiment
#'   convertGenesToSymbols decode interestingGroups interestingGroups<-
#'   mapGenesToRownames matchSampleColumn melt metrics nonzeroRowsAndCols
#'   organism sampleData
#' @importMethodsFrom AcidGenomes Gene2Symbol organism
#' @importMethodsFrom AcidPlyr leftJoin melt mutateIf
#' @importMethodsFrom AcidSingleCell aggregate aggregateCellsToSamples
#'   barcodeRanksPerSample cell2sample melt metrics sampleData zerosVsDepth
#' @importMethodsFrom pipette coerce decode
#' @importMethodsFrom syntactic camelCase makeLabel makeTitle snakeCase
NULL



## S3 generics =================================================================

#' @importFrom pipette as_tibble
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase methodFormals requireNamespaces standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertWarning toInlineString
#' @importFrom AcidExperiment matchInterestingGroups
#' @importFrom IRanges DataFrameList
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom ggplot2 aes annotation_logticks continuous_scale coord_fixed
#'   coord_flip discrete_scale element_blank element_line element_rect
#'   element_text expand_limits facet_grid facet_wrap geom_bar geom_boxplot
#'   geom_density geom_histogram geom_hline geom_jitter geom_label geom_line
#'   geom_point geom_smooth geom_step geom_text geom_violin geom_vline ggplot
#'   guides labs margin position_jitterdodge scale_colour_continuous
#'   scale_colour_discrete scale_fill_continuous scale_fill_discrete
#'   scale_x_continuous scale_x_discrete scale_y_continuous stat stat_ecdf
#'   stat_summary theme theme_linedraw waiver vars
#' @importFrom goalie allAreHexColors allAreNonNegative allArePositive
#'   areDisjointSets areSetEqual assert bapply getNameInParent hasClusters
#'   hasColnames hasDims hasLength hasMultipleSamples hasNames hasNoDuplicates
#'   hasMetrics hasNonzeroRowsAndCols hasRownames hasRows hasSubset
#'   hasUniqueCols isBiocParallelParam isCharacter isDark isFlag isGGScale
#'   isGreaterThanOrEqualTo isHexColorFunction isInClosedRange isInLeftOpenRange
#'   isInRange isInRightOpenRange isInt isNonNegative isNumber isPositive
#'   isScalar isString isSubset
#' @importFrom grDevices axisTicks colorRampPalette rgb
#' @importFrom grid arrow unit
#' @importFrom methods as formalArgs is setMethod signature validObject
#' @importFrom pipette tibble
#' @importFrom rlang !! quo quo_text sym syms
#' @importFrom scales comma gradient_n_pal log_breaks percent pretty_breaks
#' @importFrom stats as.formula dist ecdf hclust prcomp reorder
#' @importFrom stringr str_replace_na
#' @importFrom utils packageName packageVersion
NULL
