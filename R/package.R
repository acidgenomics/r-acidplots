#' AcidPlots
#'
#' Functions for plotting genomic data.
#'
#' @keywords internal
"_PACKAGE"



#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom pipette coerce
#'
#' @importFrom AcidBase formalsList intersectionMatrix methodFormals
#'   requireNamespaces sem standardizeCall
#' @importFrom AcidCLI alert alertInfo alertWarning
#' @importFrom AcidExperiment as.SummarizedExperiment convertGenesToSymbols
#'   counts decode interestingGroups interestingGroups<- mapGenesToRownames
#'   matchInterestingGroups matchSampleColumn metrics nonzeroRowsAndCols
#'   sampleData
#' @importFrom AcidGenomes Gene2Symbol organism
#' @importFrom AcidPlyr leftJoin melt
#' @importFrom AcidSingleCell aggregateCellsToSamples cell2sample zerosVsDepth
#' @importFrom IRanges DataFrameList
#' @importFrom RColorBrewer brewer.pal
#' @importFrom SummarizedExperiment SummarizedExperiment assayNames assays
#'   assays<- colData rowData
#' @importFrom S4Vectors DataFrame SimpleList aggregate head mcols merge
#'   metadata
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 aes continuous_scale coord_fixed coord_flip
#'   discrete_scale element_blank element_line element_rect element_text
#'   expand_limits facet_grid facet_wrap geom_bar geom_boxplot geom_density
#'   geom_histogram geom_hline geom_jitter geom_label geom_line geom_point
#'   geom_smooth geom_step geom_text geom_violin geom_vline ggplot guides labs
#'   margin position_jitterdodge scale_x_continuous scale_x_discrete
#'   scale_y_continuous stat_ecdf stat_summary theme theme_linedraw waiver vars
#' @importFrom goalie allAreHexColors areDisjointSets areSetEqual assert bapply
#'   getNameInParent hasDims hasLength hasNames hasNoDuplicates hasMetrics
#'   hasNonzeroRowsAndCols hasRownames hasSubset hasUniqueCols isCharacter
#'   isFlag isGGScale isGreaterThanOrEqualTo isHexColorFunction isInClosedRange
#'   isInLeftOpenRange isInRange isInRightOpenRange isInt isNonNegative isNumber
#'   isPositive isScalar isString isSubset
#' @importFrom grDevices axisTicks colorRampPalette rgb
#' @importFrom grid arrow unit
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods as formalArgs is validObject
#' @importFrom pipette as_tibble
#' @importFrom rlang !! quo quo_text sym syms
#' @importFrom scales gradient_n_pal log_breaks percent pretty_breaks
#' @importFrom stats as.formula dist ecdf hclust prcomp quantile reorder var
#' @importFrom stringr str_replace_na
#' @importFrom syntactic camelCase makeLabel makeTitle snakeCase
#' @importFrom utils packageName packageVersion
NULL
