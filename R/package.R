#' AcidPlots
#'
#' Functions for plotting genomic data.
#'
#' @keywords internal
"_PACKAGE"



## S4 classes ==================================================================

#' @importClassesFrom AcidSingleCell KnownMarkers
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors DFrame
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics GeneToSymbol aggregateCellsToSamples
#' as.SummarizedExperiment barcodeRanksPerSample calculateMetrics camelCase
#' cellCountsPerCluster cellTypesPerCluster convertGenesToSymbols
#' interestingGroups interestingGroups<- intersectionMatrix leftJoin makeLabel
#' makeTitle mapGenesToRownames mapGenesToSymbols matchSampleColumn melt
#' metrics mutateIf nonzeroRowsAndCols plotBarcodeRanks plotCellCounts
#' plotCellCountsPerCluster plotCellTypesPerCluster plotCorrelation
#' plotCorrelationHeatmap plotCounts plotCountsCorrelation
#' plotCountsCorrelationHeatmap plotCountsPerBiotype plotCountsPerBroadClass
#' plotCountsPerCell plotCountsPerFeature plotCountsVsFeatures plotDots
#' plotFeature plotFeaturesDetected plotFeaturesPerCell plotGenderMarkers
#' plotHeatmap plotKnownMarkers plotMarker plotMitoRatio plotNovelty plotPca
#' plotQc plotQuantileHeatmap plotReducedDim plotStackedBarPlot plotSums
#' plotTsne plotTotalCounts plotUpset plotUmap plotViolin plotWaterfall
#' plotZerosVsDepth sampleData sem snakeCase uniteInterestingGroups
#' zerosVsDepth
#' @importFrom Biobase sampleNames
#' @importFrom BiocGenerics append counts do.call organism rbind t unlist
#' unsplit var
#' @importFrom IRanges quantile
#' @importFrom S4Vectors aggregate complete.cases decode head mcols merge
#' metadata na.omit sort split
#' @importFrom SingleCellExperiment logcounts reducedDim reducedDimNames
#' reducedDimNames<- reducedDims
#' @importFrom SummarizedExperiment assayNames assay assay<- assays assays<-
#' colData rowData
#' @importFrom methods show
NULL

#' @importMethodsFrom AcidBase intersectionMatrix sem
#' @importMethodsFrom AcidExperiment aggregate as.SummarizedExperiment
#' calculateMetrics convertGenesToSymbols decode interestingGroups
#' interestingGroups<- mapGenesToRownames mapGenesToSymbols matchSampleColumn
#' melt metrics nonzeroRowsAndCols organism sampleData uniteInterestingGroups
#' @importMethodsFrom AcidGenomes GeneToSymbol organism
#' @importMethodsFrom AcidPlyr leftJoin melt mutateIf
#' @importMethodsFrom AcidSingleCell aggregate aggregateCellsToSamples
#' barcodeRanksPerSample cellCountsPerCluster cellTypesPerCluster
#' melt metrics sampleData zerosVsDepth
#' @importMethodsFrom pipette decode
#' @importMethodsFrom syntactic camelCase makeLabel makeTitle snakeCase
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase methodFormals printString standardizeCall strReplaceNa
#' @importFrom AcidCLI abort alert alertInfo alertWarning dl toInlineString
#' @importFrom AcidExperiment matchInterestingGroups
#' @importFrom AcidMarkdown markdownHeader
#' @importFrom IRanges DataFrameList SplitDataFrameList
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom ggplot2 aes after_stat annotation_logticks continuous_scale
#' coord_fixed coord_flip discrete_scale element_blank element_line element_rect
#' element_text expand_limits expansion facet_grid facet_wrap geom_bar
#' geom_boxplot geom_density geom_histogram geom_hline geom_jitter geom_label
#' geom_line geom_point geom_smooth geom_step geom_text geom_violin geom_vline
#' ggplot guide_colorbar guides labs margin position_jitterdodge
#' scale_colour_continuous scale_colour_discrete scale_fill_continuous
#' scale_fill_discrete scale_radius scale_x_continuous scale_x_discrete
#' scale_y_continuous stat_ecdf stat_summary theme theme_linedraw waiver vars
#' @importFrom goalie allAreHexColors allAreMatchingRegex allAreNonNegative
#' allArePositive areDisjointSets areSetEqual assert bapply hasClusters
#' hasColnames hasCols hasDims hasLength hasMultipleSamples hasNames
#' hasNoDuplicates hasMetrics hasNonzeroRowsAndCols hasRownames hasRows
#' hasSubset hasUniqueCols hasValidDimnames isAlpha isCharacter isDark isFlag
#' isGgscale isGreaterThanOrEqualTo isHeaderLevel isHexColorFunction
#' isInClosedRange isInLeftOpenRange isInRange isInRightOpenRange isInt
#' isIntegerish isMatchingRegex isNonNegative isNumber isPositive isScalar
#' isString isSubset requireNamespaces
#' @importFrom grDevices axisTicks colorRampPalette rgb
#' @importFrom grid arrow unit
#' @importFrom methods as formalArgs is setMethod signature validObject
#' @importFrom rlang !! !!! .data sym syms
#' @importFrom scales comma gradient_n_pal log_breaks percent pretty_breaks
#' @importFrom stats as.formula dist ecdf hclust median prcomp reorder
#' @importFrom utils packageName packageVersion
NULL
