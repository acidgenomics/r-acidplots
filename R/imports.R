#' @importFrom SummarizedExperiment assays assays<- rowData
#' @importFrom S4Vectors DataFrame aggregate
#' @importFrom basejump Gene2Symbol aggregateCellsToSamples assert
#'   as.SummarizedExperiment as_tibble camel cell2sample convertGenesToSymbols
#'   counts decode formalsList interestingGroups interestingGroups<-
#'   matchArgsToDoCall matchInterestingGroups mapGenesToRownames meltCounts
#'   metrics organism sampleData snake standardizeCall zerosVsDepth
#' @importFrom cowplot plot_grid
#' @importFrom dplyr arrange bind_rows case_when desc filter funs group_by
#'   left_join mutate mutate_all mutate_at mutate_if n pull rename select
#'   select_if slice summarise summarise_all top_n ungroup
#' @importFrom ggplot2 aes coord_fixed coord_flip element_blank element_line
#'   element_rect element_text expand_limits facet_wrap geom_bar geom_boxplot
#'   geom_density geom_hline geom_jitter geom_label geom_point geom_violin
#'   geom_vline ggplot guides labs position_jitterdodge scale_x_continuous
#'   scale_y_continuous stat_ecdf stat_summary theme theme_linedraw
#' @importFrom ggrepel geom_label_repel
#' @importFrom goalie hasUniqueCols isCharacter isFlag isGGScale isInClosedRange
#'   isInt isNonNegative isNumber isPositive isScalar isString isSubset
#' @importFrom grid arrow unit
#' @importFrom magrittr %>%
#' @importFrom matrixStats rowVars
#' @importFrom methods as is validObject
#' @importFrom rlang := UQ sym syms
#' @importFrom stats as.formula prcomp
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom utils data globalVariables
NULL
