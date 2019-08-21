## DropletUtils isn't supported on R 3.5.
## #' @importFrom DropletUtils barcodeRanks



#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom IRanges DataFrameList
#' @importFrom RColorBrewer brewer.pal
#' @importFrom SummarizedExperiment SummarizedExperiment assayNames assays
#'   assays<- colData rowData
#' @importFrom S4Vectors DataFrame aggregate metadata
#' @importFrom UpSetR upset
#' @importFrom basejump Gene2Symbol aggregateCellsToSamples
#'   as.SummarizedExperiment as_tibble camelCase cell2sample
#'   convertGenesToSymbols counts decode formalsList interestingGroups
#'   interestingGroups<- left_join makeLabel makeTitle matchArgsToDoCall
#'   matchInterestingGroups mapGenesToRownames meltCounts methodFormals metrics
#'   organism sampleData snakeCase standardizeCall zerosVsDepth
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 aes continuous_scale coord_fixed coord_flip
#'   discrete_scale element_blank element_line element_rect element_text
#'   expand_limits expand_scale facet_wrap geom_bar geom_boxplot geom_density
#'   geom_histogram geom_hline geom_jitter geom_label geom_line geom_point
#'   geom_smooth geom_step geom_violin geom_vline ggplot guides labs
#'   position_jitterdodge scale_x_continuous scale_x_discrete scale_y_continuous
#'   stat_ecdf stat_summary theme theme_linedraw waiver
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggridges geom_density_ridges
#' @importFrom goalie allAreHexColors areDisjointSets areSetEqual assert bapply
#'   getNameInParent hasDims hasLength hasNames hasNoDuplicates hasMetrics
#'   hasNonZeroRowsAndCols hasRownames hasSubset hasUniqueCols isCharacter
#'   isFlag isGGScale isGreaterThanOrEqualTo isHexColorFunction isInClosedRange
#'   isInLeftOpenRange isInRange isInRightOpenRange isInt isNonNegative isNumber
#'   isPositive isScalar isString isSubset
#' @importFrom grDevices axisTicks colorRampPalette
#' @importFrom grid arrow unit
#' @importFrom matrixStats colVars rowVars
#' @importFrom methods as formalArgs is validObject
#' @importFrom pheatmap pheatmap
#' @importFrom rlang !! quo_text sym syms
#' @importFrom scales gradient_n_pal log_breaks percent pretty_breaks
#' @importFrom stats as.formula dist ecdf hclust prcomp quantile
#' @importFrom utils data globalVariables packageVersion
#'
#'
#'
#' @importFrom dplyr mutate mutate_all select
#' @importFrom magrittr %>%
NULL
