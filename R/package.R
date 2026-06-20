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
#' @importFrom AcidGenerics as.SummarizedExperiment barcodeRanksPerSample
#' @importFrom AcidGenerics calculateMetrics camelCase cellCountsPerCluster
#' @importFrom AcidGenerics cellTypesPerCluster convertGenesToSymbols
#' @importFrom AcidGenerics interestingGroups interestingGroups<-
#' @importFrom AcidGenerics intersectionMatrix leftJoin makeLabel makeTitle
#' @importFrom AcidGenerics mapGenesToRownames mapGenesToSymbols
#' @importFrom AcidGenerics matchSampleColumn melt metrics mutateIf
#' @importFrom AcidGenerics nonzeroRowsAndCols plotBarcodeRanks
#' @importFrom AcidGenerics plotCellCounts plotCellCountsPerCluster
#' @importFrom AcidGenerics plotCellTypesPerCluster plotCorrelation
#' @importFrom AcidGenerics plotCorrelationHeatmap plotCounts
#' @importFrom AcidGenerics plotCountsCorrelation
#' @importFrom AcidGenerics plotCountsCorrelationHeatmap
#' @importFrom AcidGenerics plotCountsPerBiotype plotCountsPerBroadClass
#' @importFrom AcidGenerics plotCountsPerCell plotCountsPerFeature
#' @importFrom AcidGenerics plotCountsVsFeatures plotDots plotFeature
#' @importFrom AcidGenerics plotFeaturesDetected plotFeaturesPerCell
#' @importFrom AcidGenerics plotGenderMarkers plotHeatmap plotKnownMarkers
#' @importFrom AcidGenerics plotMarker plotMitoRatio plotNovelty plotPca
#' @importFrom AcidGenerics plotQc plotQuantileHeatmap plotReducedDim
#' @importFrom AcidGenerics plotStackedBarPlot plotSums plotTsne
#' @importFrom AcidGenerics plotTotalCounts plotUpset plotUmap plotViolin
#' @importFrom AcidGenerics plotWaterfall plotZerosVsDepth sampleData sem
#' @importFrom AcidGenerics snakeCase uniteInterestingGroups zerosVsDepth
#' @importFrom Biobase sampleNames
#' @importFrom BiocGenerics append counts do.call organism rbind t unlist
#' @importFrom BiocGenerics unsplit var
#' @importFrom IRanges quantile
#' @importFrom S4Vectors aggregate complete.cases decode head mcols merge
#' @importFrom S4Vectors metadata na.omit sort split
#' @importFrom SingleCellExperiment logcounts reducedDim reducedDimNames
#' @importFrom SingleCellExperiment reducedDimNames<- reducedDims
#' @importFrom SummarizedExperiment assayNames assay assay<- assays assays<-
#' @importFrom SummarizedExperiment colData rowData
#' @importFrom methods show
NULL

#' @importMethodsFrom AcidBase intersectionMatrix sem
#' @importMethodsFrom AcidExperiment aggregate as.SummarizedExperiment
#' @importMethodsFrom AcidExperiment calculateMetrics convertGenesToSymbols
#' @importMethodsFrom AcidExperiment decode interestingGroups
#' @importMethodsFrom AcidExperiment interestingGroups<- mapGenesToRownames
#' @importMethodsFrom AcidExperiment mapGenesToSymbols matchSampleColumn
#' @importMethodsFrom AcidExperiment melt metrics nonzeroRowsAndCols
#' @importMethodsFrom AcidExperiment organism sampleData uniteInterestingGroups
#' @importMethodsFrom AcidGenomes GeneToSymbol organism
#' @importMethodsFrom AcidPlyr leftJoin melt mutateIf
#' @importMethodsFrom AcidSingleCell aggregate aggregateCellsToSamples
#' @importMethodsFrom AcidSingleCell barcodeRanksPerSample
#' @importMethodsFrom AcidSingleCell cellCountsPerCluster cellTypesPerCluster
#' @importMethodsFrom AcidSingleCell melt metrics sampleData zerosVsDepth
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
#' @importFrom ggplot2 coord_fixed coord_flip discrete_scale element_blank
#' @importFrom ggplot2 element_line element_rect element_text expand_limits
#' @importFrom ggplot2 expansion facet_grid facet_wrap geom_bar geom_boxplot
#' @importFrom ggplot2 geom_density geom_histogram geom_hline geom_jitter
#' @importFrom ggplot2 geom_label geom_line geom_point geom_smooth geom_step
#' @importFrom ggplot2 geom_text geom_violin geom_vline ggplot guide_colorbar
#' @importFrom ggplot2 guides labs margin position_jitterdodge
#' @importFrom ggplot2 scale_colour_continuous scale_colour_discrete
#' @importFrom ggplot2 scale_fill_continuous scale_fill_discrete scale_radius
#' @importFrom ggplot2 scale_x_continuous scale_x_discrete scale_y_continuous
#' @importFrom ggplot2 stat_ecdf stat_summary theme theme_linedraw waiver vars
#' @importFrom goalie allAreHexColors allAreMatchingRegex allAreNonNegative
#' @importFrom goalie allArePositive areDisjointSets areSetEqual assert bapply
#' @importFrom goalie hasClusters hasColnames hasCols hasDims hasLength
#' @importFrom goalie hasMultipleSamples hasNames hasNoDuplicates hasMetrics
#' @importFrom goalie hasNonzeroRowsAndCols hasRownames hasRows hasSubset
#' @importFrom goalie hasUniqueCols hasValidDimnames isAlpha isCharacter
#' @importFrom goalie isDark isFlag isGgscale isGreaterThanOrEqualTo
#' @importFrom goalie isHeaderLevel isHexColorFunction isInClosedRange
#' @importFrom goalie isInLeftOpenRange isInRange isInRightOpenRange isInt
#' @importFrom goalie isIntegerish isMatchingRegex isNonNegative isNumber
#' @importFrom goalie isPositive isScalar isString isSubset requireNamespaces
#' @importFrom grDevices axisTicks colorRampPalette rgb
#' @importFrom grid arrow unit
#' @importFrom methods as formalArgs is setMethod signature validObject
#' @importFrom rlang !! !!! .data sym syms
#' @importFrom scales comma gradient_n_pal log_breaks percent pretty_breaks
#' @importFrom stats as.formula dist ecdf hclust median prcomp reorder
#' @importFrom utils packageName packageVersion
NULL
