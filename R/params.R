#' Parameters
#'
#' @name params
#' @keywords internal
#'
#' @param alpha `numeric(1)`.
#'   Adjusted P value ("alpha") cutoff. If left `NULL`, will use the cutoff
#'   defined in the object.
#' @param aspectRatio `integer(1)`.
#'   Aspect ratio.
#' @param assay `vector(1)`.
#'   Name or index of count matrix slotted in
#'   [`assays()`][SummarizedExperiment::assays]. When passing in a string, the
#'   name must be defined in
#'   [`assayNames()`][SummarizedExperiment::assayNames].
#' @param assays `list`.
#'   Count matrices, which must have matching dimensions. Counts can be passed
#'   in as either a dense matrix (`matrix`) or sparse matrix (`sparseMatrix`).
#' @param bpparam `bpparamClass`.
#'   BiocParallel parameter specifying the back-end to be used for computations.
#'
#'   We recommend using one of the following:
#'
#'   - [bpparam][BiocParallel::bpparam].
#'   - [SerialParam][BiocParallel::SerialParam].
#'   - [MulticoreParam][BiocParallel::MulticoreParam].
#' @param censorSamples `character`.
#'   Specify a subset of samples to censor.
#' @param check `logical(1)`.
#'   Perform assert checks.
#' @param colData `DataFrame`.
#'   Metadata describing the assay columns. For bulk RNA-seq, this data
#'   describes the samples. For single-cell RNA-seq, this data describes the
#'   cells.
#' @param colnames `logical(1)`.
#'   Apply to column names.
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
#' @param counts `matrix`.
#'   Count matrix. Normalized counts are recommended.
#' @param countsAxisLabel `character(1)`.
#'   Counts axis label.
#' @param dark `logical(1)`.
#'   Plot against a dark background using the
#'   [`theme_midnight()`][minimalism::theme_midnight] ggplot2 theme.
#' @param dimsUse `integer`.
#'   Vector of length 2 that denotes the columns from the reduced dimension
#'   matrix to use for `centerX` and `centerY` column calculations. Defaults the
#'   first and second dimensions.
#' @param dir `character(1)`.
#'   Directory path.
#' @param direction `character(1)`.
#'   Plot "`both`", "`up`", or "`down`" directions.
#' @param ensemblRelease `integer(1)`.
#'   Ensembl release version (e.g. `90`). We recommend setting this value if
#'   possible, for improved reproducibility. When left unset, the latest release
#'   available via AnnotationHub/ensembldb is used. Note that the latest version
#'   available can vary, depending on the versions of AnnotationHub and
#'   ensembldb in use.
#' @param envir `environment`.
#'   Environment to use for assignment. Defaults to
#'   [`parent.frame()`][base::parent.frame], which will assign into the calling
#'   environment.
#' @param expression `character(1)`.
#'   Calculation to apply.
#'   Uses [`match.arg()`][base::match.arg] internally and defaults to the first
#'   argument in the `character` vector.
#' @param file `character(1)`.
#'   File path.
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
#' @param genomeBuild `character(1)`.
#'   Ensembl genome build assembly name (e.g. `"GRCh38"`). If set `NULL`,
#'   defaults to the most recent build available. Note: don't pass in UCSC build
#'   IDs (e.g. `"hg38"`).
#' @param geom `character(1)`.
#'   Plot type. Uses [`match.arg()`][base::match.arg] internally and defaults to
#'   the first argument in the `character` vector.
#' @param gffFile `character(1)`.
#'   GFF/GTF (General Feature Format) file. Generally, we recommend using a GTF
#'   (GFFv2) instead of a GFFv3 file if possible.
#' @param headerLevel `integer(1)` (`1`-`7`).
#'   Markdown header level.
#' @param i
#'   Indices specifying elements to extract or replace. Indices are `numeric` or
#'   `character` vectors, empty (`missing`), or `NULL`.
#'
#'   For more information:
#'
#'   ```
#'   help(topic = "Extract", package = "base")
#'   ```
#' @param inherits `logical(1)`.
#'   Should the enclosing frames of the `environment` be searched?
#' @param interestingGroups `character`.
#'   Groups of interest that define the samples. If left unset, defaults to
#'   `sampleName`.
#' @param label `logical(1)`.
#'   Superimpose sample text labels on the plot.
#' @param labelSize `integer(1)`.
#'   Size of the text label.
#' @param lfcThreshold `numeric(1)`.
#'   Log fold change ratio (base 2) cutoff threshold. If left `NULL`, will use
#'   the cutoff defined in the object.
#' @param legend `logical(1)`.
#'   Show plot legend.
#' @param level `character(1)`.
#'   Return as genes or transcripts.
#' @param limit `numeric(1)`.
#'   Threshold to denote on the plot, using a dashed line.
#' @param max `numeric(1)`.
#'   Recommended maximum value cutoff.
#' @param metadata `list`.
#'   Metadata.
#' @param min `numeric(1)`.
#'   Recommended minimum value cutoff.
#' @param minCounts `integer(1)`.
#'   Minimum number of counts per gene in the count matrix.
#' @param n `integer(1)`.
#'   Number to include.
#' @param ntop `integer(1)`.
#'   Number of top genes to label.
#' @param object Object.
#' @param organism `character(1)`.
#'   Full Latin organism name (e.g. "`Homo sapiens`").
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
#' @param prefilter `logical(1)`.
#'   Apply prefiltering to remove zero count genes.
#' @param progress `logical(1)`.
#'   Show progress, using progress bars.
#' @param reducedDim `character(1)`.
#'   Name of reduced dimension matrix slotted in
#'   [`reducedDims()`][SingleCellExperiment::reducedDims].
#'   Includes TNSE, UMAP, PCA, for example.
#' @param return `character(1)`.
#'   Return type. Uses [`match.arg()`][base::match.arg] internally and defaults
#'   to the first argument in the `character` vector.
#' @param rowData `DataFrame`.
#'   Metadata describing the assay rows, if genomic ranges are not available.
#'   *Use rowRanges (GRanges) instead, if possible*.
#' @param rowRanges `GRanges`.
#'   Genomic ranges (e.g. genome annotations). Metadata describing the assay
#'   rows.
#' @param rownames `logical(1)`.
#'   Apply to row names.
#' @param sampleMetadataFile `character(1)`.
#'   Sample metadata file path. CSV or TSV is preferred, but Excel worksheets
#'   are also supported. Check the documentation for conventions and required
#'   columns.
#' @param samples `character`.
#'   Specify a subset of samples to include.
#' @param sigPointColor `character`.
#'   Color names for labeling upregulated and downregulated genes. Also supports
#'   a character string for labeling DEGs with the same color, regardless of
#'   direction.
#' @param sort `logical(1)`.
#'   Resort using `sort`.
#' @param spikeNames `character`.
#'   Vector indicating which assay rows denote spike-in sequences (e.g. ERCCs).
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
#' @param transgeneNames `character`.
#'   Vector indicating which assay rows denote transgenes (e.g. EGFP, TDTOMATO).
#' @param trendline `logical(1)`.
#'   Include trendline on plot.
#' @param tx2gene `Tx2Gene`.
#'   Transcript-to-gene mappings.
#' @param url `character(1)`.
#'   Uniform Resource Locator (URL). HTTP or FTP address.
#' @param value Value to assign.
#' @param verbose `logical(1)`.
#'   Run the function with verbose messages?
#'   *It is only recommended to enable this when debugging.*
#' @param x Object.
#' @param ... Additional arguments.
#'
#' @return No value.
NULL
