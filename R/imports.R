## DropletUtils is a pretty heavy dependency that can fail to compile on some
## Linux distros. Keep it as a suggested package, rather than importing.
## > #' @importFrom DropletUtils barcodeRanks



#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#'
#' @importMethodsFrom basejump coerce
#'
#' @importFrom IRanges DataFrameList
#' @importFrom RColorBrewer brewer.pal
#' @importFrom SummarizedExperiment SummarizedExperiment assayNames assays
#'   assays<- colData rowData
#' @importFrom S4Vectors DataFrame SimpleList aggregate head merge metadata
#' @importFrom UpSetR upset
#' @importFrom basejump Gene2Symbol aggregateCellsToSamples
#'   as.SummarizedExperiment as_tibble camelCase cell2sample
#'   convertGenesToSymbols counts decode formalsList interestingGroups
#'   interestingGroups<- leftJoin makeLabel makeTitle matchInterestingGroups
#'   mapGenesToRownames methodFormals melt metrics nonzeroRowsAndCols organism
#'   sampleData snakeCase standardizeCall zerosVsDepth
#' @importFrom cli cli_alert cli_alert_info cli_alert_warning
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 aes continuous_scale coord_fixed coord_flip
#'   discrete_scale element_blank element_line element_rect element_text
#'   expand_limits facet_grid facet_wrap geom_bar geom_boxplot geom_density
#'   geom_histogram geom_hline geom_jitter geom_label geom_line geom_point
#'   geom_smooth geom_step geom_text geom_violin geom_vline ggplot guides labs
#'   margin position_jitterdodge scale_x_continuous scale_x_discrete
#'   scale_y_continuous stat_ecdf stat_summary theme theme_linedraw waiver vars
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggridges geom_density_ridges
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
#' @importFrom pheatmap pheatmap
#' @importFrom rlang !! quo quo_text sym syms
#' @importFrom scales gradient_n_pal log_breaks percent pretty_breaks
#' @importFrom stats as.formula dist ecdf hclust prcomp quantile reorder var
#' @importFrom stringr str_replace_na
#' @importFrom utils data packageVersion
NULL
