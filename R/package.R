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
#' @importFrom basejump DataFrame DataFrameList Gene2Symbol SimpleList
#'   SummarizedExperiment aggregate aggregateCellsToSamples alert alertInfo
#'   alertWarning arrow as as_tibble as.SummarizedExperiment as.formula
#'   assayNames assay assay<- assays assays<- axisTicks camelCase cell2sample
#'   colData colorRampPalette complete.cases convertGenesToSymbols counts decode dist ecdf
#'   formalArgs formalsList hclust head interestingGroups interestingGroups<-
#'   intersectionMatrix is leftJoin makeLabel makeTitle mapGenesToRownames
#'   matchInterestingGroups matchSampleColumn mcols melt merge metadata
#'   methodFormals metrics organism packageName packageVersion prcomp quantile
#'   reorder requireNamespaces sem nonzeroRowsAndCols rgb rowData sampleData
#'   snakeCase standardizeCall unit validObject var zerosVsDepth
#' @importFrom goalie allAreHexColors areDisjointSets areSetEqual assert bapply
#'   getNameInParent hasDims hasLength hasNames hasNoDuplicates hasMetrics
#'   hasNonzeroRowsAndCols hasRownames hasSubset hasUniqueCols isCharacter
#'   isFlag isGGScale isGreaterThanOrEqualTo isHexColorFunction isInClosedRange
#'   isInLeftOpenRange isInRange isInRightOpenRange isInt isNonNegative isNumber
#'   isPositive isScalar isString isSubset
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 aes continuous_scale coord_fixed coord_flip
#'   discrete_scale element_blank element_line element_rect element_text
#'   expand_limits facet_grid facet_wrap geom_bar geom_boxplot geom_density
#'   geom_histogram geom_hline geom_jitter geom_label geom_line geom_point
#'   geom_smooth geom_step geom_text geom_violin geom_vline ggplot guides labs
#'   margin position_jitterdodge scale_x_continuous scale_x_discrete
#'   scale_y_continuous stat_ecdf stat_summary theme theme_linedraw waiver vars
#' @importFrom matrixStats colVars rowVars
#' @importFrom rlang !! quo quo_text sym syms
#' @importFrom scales comma gradient_n_pal log_breaks percent pretty_breaks
#' @importFrom stringr str_replace_na
NULL
