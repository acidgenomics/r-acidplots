#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param aspectRatio `integer(1)`.
#'   Aspect ratio.
#' @param assay `vector(1)`.
#'   Name or index of count matrix slotted in
#'   [`assays()`][SummarizedExperiment::assays]. When passing in a string, the
#'   name must be defined in
#'   [`assayNames()`][SummarizedExperiment::assayNames].
#' @param color `ScaleDiscrete`.
#'   Desired ggplot2 color scale. Must supply discrete values. When set `NULL`,
#'   the default ggplot2 color palette will be used. If manual color definitions
#'   are desired, we recommend using [ggplot2::scale_color_manual()].
#'
#'   To set the discrete color palette globally, use:
#'
#'   ```
#'   options(acid.color.discrete = ggplot2::scale_color_viridis_d())
#'   ```
#' @param countsAxisLabel `character(1)`.
#'   Counts axis label.
#' @param dark `logical(1)`.
#'   Plot against a dark background using the
#'   [`theme_midnight()`][minimalism::theme_midnight] ggplot2 theme.
#' @param dimsUse `integer`.
#'   Vector of length 2 that denotes the columns from the reduced dimension
#'   matrix to use for `centerX` and `centerY` column calculations. Defaults the
#'   first and second dimensions.
#' @param direction `character(1)`.
#'   Plot "`both`", "`up`", or "`down`" directions.
#' @param expression `character(1)`.
#'   Calculation to apply.
#'   Uses [`match.arg()`][base::match.arg] internally and defaults to the first
#'   argument in the `character` vector.
#' @param fill `ggproto`/`ScaleDiscrete`.
#'   Desired ggplot2 fill scale. Must supply discrete values. When set to
#'   `NULL`, the default ggplot2 color palette will be used. If manual color
#'   definitions are desired, we recommend using [ggplot2::scale_fill_manual()].
#'
#'   To set the discrete fill palette globally, use:
#'
#'   ```
#'   options(acid.fill.discrete = ggplot2::scale_fill_viridis_d())
#'   ```
#' @param flip `logical(1)`.
#'   Flip x and y axes. Recommended for plots containing many samples.
#' @param gene2symbol `Gene2Symbol`.
#'   Gene-to-symbol mappings. Must contain `geneID` and `geneName` columns. See
#'   `Gene2Symbol` for more information.
#' @param genes `character`.
#'   Gene identifiers. It is considered better practice to input the stable gene
#'   identifiers from Ensembl (e.g. "ENSG00000000003") and not the (HGNC) gene
#'   symbols (e.g. "TSPN6"), if possible.
#' @param geom `character(1)`.
#'   Plot type. Uses [`match.arg()`][base::match.arg] internally and defaults to
#'   the first argument in the `character` vector.
#' @param headerLevel `integer(1)` (`1`-`7`).
#'   Markdown header level.
#' @param interestingGroups `character`.
#'   Groups of interest that define the samples. If left unset, defaults to
#'   `sampleName`.
#' @param label `logical(1)`.
#'   Superimpose sample text labels on the plot.
#' @param labelSize `integer(1)`.
#'   Size of the text label.
#' @param legend `logical(1)`.
#'   Show plot legend.
#' @param level `character(1)`.
#'   Return as genes or transcripts.
#' @param limit `numeric(1)`.
#'   Threshold to denote on the plot, using a dashed line.
#' @param max `numeric(1)`.
#'   Recommended maximum value cutoff.
#' @param min `numeric(1)`.
#'   Recommended minimum value cutoff.
#' @param minCounts `integer(1)`.
#'   Minimum number of counts per gene in the count matrix.
#' @param n `integer(1)`.
#'   Number to include.
#' @param ntop `integer(1)`.
#'   Number of top genes to label.
#' @param object Object.
#' @param perMillion `logical(1)`.
#'   Display as counts per million.
#' @param perSample `logical(1)`.
#'   Visualize the distributions per sample.
#' @param plotlist `list`.
#'   List containing `ggplot` objects.
#' @param pointAlpha `numeric(1)` (`0`-`1`).
#'   Alpha transparency level. Useful when there many points in the dataset
#'   (e.g. single-cell data), and some points can be masked.
#' @param pointColor `character(1)`.
#'   Default point color for the plot.
#' @param pointScalar `integer(1)`.
#'   Default point size for the plot.
#' @param pointsAsNumbers `logical(1)`.
#'   Plot the points as numbers (`TRUE`) or dots (`FALSE`).
#' @param pointSize `numeric(1)`.
#'   Point size for dots in the plot.
#' @param return `character(1)`.
#'   Return type. Uses [`match.arg()`][base::match.arg] internally and defaults
#'   to the first argument in the `character` vector.
#' @param sigPointColor `character`.
#'   Color names for labeling upregulated and downregulated genes. Also supports
#'   a character string for labeling DEGs with the same color, regardless of
#'   direction.
#' @param subtitle `character(1)`.
#'   Plot subtitle.
#' @param title `character(1)`.
#'   Plot title.
#' @param trans `character(1)`.
#'   Name of the axis scale transformation to apply.
#'
#'   For more information:
#'
#'   ```
#'   help(topic = "scale_x_continuous", package = "ggplot2")
#'   ```
#' @param trendline `logical(1)`.
#'   Include trendline on plot.
#' @param ... Additional arguments.
#'
#' @return No value.
NULL
