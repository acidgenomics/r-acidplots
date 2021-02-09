#' AcidPlots
#'
#' Functions for plotting genomic data.
#'
#' @keywords internal
"_PACKAGE"



#' @importClassesFrom basejump DataFrame Matrix SingleCellExperiment
#'   SummarizedExperiment
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom basejump DataFrame DataFrameList Gene2Symbol SimpleList
#'   SummarizedExperiment aggregate aggregateCellsToSamples alert alertInfo
#'   alertWarning arrow as as_tibble as.SummarizedExperiment as.formula
#'   assayNames assay assay<- assays assays<- axisTicks camelCase cell2sample
#'   colData colorRampPalette complete.cases convertGenesToSymbols counts decode
#'   dist ecdf formalArgs formalsList hclust head interestingGroups
#'   interestingGroups<- intersectionMatrix is leftJoin makeLabel makeTitle
#'   mapGenesToRownames matchInterestingGroups matchSampleColumn mcols melt
#'   merge metadata methodFormals metrics organism packageName packageVersion
#'   prcomp quantile reorder requireNamespaces sem nonzeroRowsAndCols rgb
#'   rowData sampleData snakeCase standardizeCall tibble unit validObject var
#'   zerosVsDepth
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 aes annotation_logticks continuous_scale coord_fixed
#'   coord_flip discrete_scale element_blank element_line element_rect
#'   element_text expand_limits facet_grid facet_wrap geom_bar geom_boxplot
#'   geom_density geom_histogram geom_hline geom_jitter geom_label geom_line
#'   geom_point geom_smooth geom_step geom_text geom_violin geom_vline ggplot
#'   guides labs margin position_jitterdodge scale_x_continuous scale_x_discrete
#'   scale_y_continuous stat stat_ecdf stat_summary theme theme_linedraw waiver
#'   vars
#' @importFrom goalie allAreHexColors allArePositive areDisjointSets areSetEqual
#'   assert bapply getNameInParent hasColnames hasDims hasLength hasNames
#'   hasNoDuplicates hasMetrics hasNonzeroRowsAndCols hasRownames hasRows
#'   hasSubset hasUniqueCols isCharacter isFlag isGGScale isGreaterThanOrEqualTo
#'   isHexColorFunction isInClosedRange isInLeftOpenRange isInRange
#'   isInRightOpenRange isInt isNonNegative isNumber isPositive isScalar
#'   isString isSubset
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods setMethod signature
#' @importFrom rlang !! quo quo_text sym syms
#' @importFrom scales comma gradient_n_pal log_breaks percent pretty_breaks
#' @importFrom stringr str_replace_na
NULL
