#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom DropletUtils barcodeRanks
#' @importFrom RColorBrewer brewer.pal
#' @importFrom SummarizedExperiment SummarizedExperiment assays assays<- colData
#'   rowData
#' @importFrom S4Vectors DataFrame aggregate
#' @importFrom UpSetR upset
#' @importFrom basejump Gene2Symbol aggregateCellsToSamples
#'   as.SummarizedExperiment as_tibble camelCase cell2sample
#'   convertGenesToSymbols counts decode formalsList interestingGroups
#'   interestingGroups<- makeLabel makeTitle matchArgsToDoCall
#'   matchInterestingGroups mapGenesToRownames meltCounts methodFormals metrics
#'   organism sampleData snakeCase standardizeCall zerosVsDepth
#' @importFrom cowplot plot_grid
#' @importFrom dplyr arrange bind_rows case_when desc filter group_by left_join
#'   mutate mutate_all mutate_at mutate_if n pull rename select select_if slice
#'   summarise summarise_all top_n ungroup
#' @importFrom ggplot2 aes continuous_scale coord_fixed coord_flip
#'   discrete_scale element_blank element_line element_rect element_text
#'   expand_limits expand_scale facet_wrap geom_bar geom_boxplot geom_density
#'   geom_hline geom_jitter geom_label geom_point geom_violin geom_vline ggplot
#'   guides labs position_jitterdodge scale_x_continuous scale_x_discrete
#'   scale_y_continuous stat_ecdf stat_summary theme theme_linedraw waiver
#' @importFrom ggrepel geom_label_repel
#' @importFrom goalie allAreHexColors areDisjointSets assert hasDims hasLength
#'   hasNames hasNoDuplicates hasNonZeroRowsAndCols hasRownames hasUniqueCols
#'   isCharacter isFlag isGGScale isGreaterThanOrEqualTo isHexColorFunction
#'   isInClosedRange isInt isNonNegative isNumber isPositive isScalar isString
#'   isSubset
#' @importFrom grDevices axisTicks colorRampPalette
#' @importFrom grid arrow unit
#' @importFrom magrittr %>%
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods as formalArgs is validObject
#' @importFrom pheatmap pheatmap
#' @importFrom rlang := UQ quo_text sym syms
#' @importFrom scales gradient_n_pal log_breaks pretty_breaks
#' @importFrom stats as.formula dist hclust prcomp quantile
#' @importFrom tibble column_to_rownames tibble
#' @importFrom tidyr gather
#' @importFrom utils data globalVariables
NULL
