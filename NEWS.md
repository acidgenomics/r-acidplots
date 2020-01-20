## acidplots 0.2.22 (2020-01-20)

### Minor changes

- Updated bioverbs dependency to renamed acidgenerics package.

## acidplots 0.2.21 (2019-12-13)

### Minor changes

- `plotHeatmap`: Increased the default break range from `-2:2` up to `-3:3`.
  This helps improve the dynamic range a bit for some RNA-seq plots.

## acidplots 0.2.20 (2019-12-09)

### Minor changes

- Hardened user-defined `interestingGroups` input against `NA` values causing
  values to drop in plots. This was discovered with `plotPCA` calling
  `geom_point` internally, which caused removal of rows. If we coerce `NA` value
  to `"NA"` first, then removal no longer occurs. This is accomplished
  internally using `str_remove_na` (see also `stringi_remove_na`). The change
  applies to these functions: `plotCellCounts`, `plotCounts`,
  `plotCountsPerBiotype`, `plotCountsPerBroadClass`, `plotCountsPerFeature`,
  `plotFeaturesDetected`, `plotPCA`, `plotTotalCounts`, `plotZerosVsDepth`.

## acidplots 0.2.19 (2019-11-19)

### Minor changes

- `plotCounts`: The `gene` argument is now optional, supporting subset
  `SummarizedExperiment` objects. Currently a maximum of 20 genes (rows) are
  allowed to pass through in the method.
- Updated documentation to support roxygen2 7.0 release.
- Improved default color palettes for `plotHeatmap`, `plotCorrelationHeatmap`,
  and `plotQuantileHeatmap`.

## acidplots 0.2.18 (2019-11-12)

### Minor changes

- `plotCounts`: Reworked `medianLine` argument  and renamed to simply `line`.
  Now supports mean and geometric mean in addition to median now.
  Line is now disabled by default.

## acidplots 0.2.17 (2019-11-07)

### Minor changes

- Updated dependencies to require Bioconductor 3.10 release.
- Added return value for `matchLabels` in documentation.

## acidplots 0.2.16 (2019-09-17)

### Major changes

- Replaced `title` argument with `labels` argument for functions that render
  plots using ggplot2.
- Now exporting `matchLabels` function, that handles internal ggplot2 label
  matching, and auto-populates with defaults in formal argument.
- Using "samplesAxis" and "countsAxis" labels for ggplot2 return that support
  multiple geoms. Consider a similar approach for QC metric plots.
  
### Minor changes

- Simplified internal handoff from `SingleCellExperiment` S4 methods to
  `SummarizedExperiment`, using standard `...` passthrough instead of relying
  upon `matchArgsToDoCall`.
- Improved color palette consistency in plotting functions.
- `plotCounts`: Moved `DESeqDataSet` method to DESeqAnalysis package.

## acidplots 0.2.15 (2019-09-13)

### Major changes

- `acid_theme_dracula`: New dark mode theme supporting Dracula color palette.
- Exporting color palette vectors: `lightPalette`, `darkPalette`,
  `draculaPalette`, `iOSLightPalette`, `iOSDarkPalette`, `macOSLightPalette`,
  `macOSDarkPalette`.
- `synesthesia`: Simplified color palette to use purple, blue, green, orange.
  User can tweak the palette using a new `palette` argument.

## acidplots 0.2.14 (2019-08-27)

### Minor changes

- `plotCountsPerFeature`: Renamed formals to match conventions in new basejump
  `melt` methods.
- Updated basejump dependencies and NAMESPACE.
- Tightened up if/then checks, ensuring boolean flag return.
- Improved some working examples and unit tests.

## acidplots 0.2.13 (2019-08-21)

### Minor changes

- `plotPCA`: Improved automatic `ntop` argument handling when passing in a
  value greater than the number or features (rows) defined in the object.
- Improved documentation for `SingleCellExperiment` methods that need to average
  or aggregate expression at sample level.
- Removed dplyr and magrittr dependencies.

## acidplots 0.2.12 (2019-08-20)

### Minor changes

- Improved consistency of internal tibble handling inside plot functions.
- Updated basejump dependency versions.

## acidplots 0.2.11 (2019-08-12)

### Major changes

- Migrated some QC plotting functions previously defined in bcbioSingleCell
  package. This makes it possible to share the code for new Chromium package,
  designed for importing single cell RNA-seq data from 10X Genomics.

## acidplots 0.2.10 (2019-08-06)

### Minor changes

- Improved documentation consistency by importing shared parameter roxygen
  from the new acidroxygen package.
- Updated basejump dependency versions.

## acidplots 0.2.9 (2019-07-30)

### Minor changes

- Updated basejump dependency versions.
- Updated some unit tests to reflect RangedSummarizedExperiment example update
  in acidtest package.

## acidplots 0.2.8 (2019-07-29)

### Minor changes

- Improved warning suppression for partial match in `pheatmap` calls.
- Improved documentation.
- Improving automatic plot title and label handling with new `makeLabel` and
  `makeTitle` functions from syntactic update.

## acidplots 0.2.7 (2019-07-23)

### New functions

- `plotCountsCorrelation` and `plotCountsCorrelationHeatmap`: New functions
  optimized for plotting correlation between gene expression matrices.
  Particularly useful for comparing pseudoaligned vs. aligned counts, which is
  implemented in the new bcbioRNASeq package update.

### Minor changes

- Improved naming consistency of internal S4 functions.
- Updated basejump dependencies.

## acidplots 0.2.6 (2019-07-18)

### Minor changes

- `plotCountsPerBiotype`: Internal tidyeval bug fix for call to `dplyr::top_n`.
  Previously, the `n` argument supported direct variable input, but now it
  must be unquoted, due to change in underlying rlang engine following the
  0.4 update.

## acidplots 0.2.5 (2019-07-17)

### Minor changes

- `upset`: Compatibility update for UpSetR v1.4 release.
- Compatibility fixes for basejump v0.10.11 release.

## acidplots 0.2.4 (2019-05-08)

### Minor changes

- Improved log2 and log10 axis scale handling for `plotCountsPerBiotype` and
  `plotCountsPerBroadClass`. Previously, the `breaks` argument was defined in
  internal `scale_y_continuous` ggplot2 call, but this doesn't perform well for
  many RNA-seq datasets at log2 scale. Instead, it's better to log2 scale the
  data first and then simplify indicate log2 on the y axis title.
- `plotCountsPerFeature`: Improved custom color handling for density geom.
  Removed violin geom support but added jitter geom support. The violin
  plot method doesn't scale well for multiple samples, and the boxplot is
  visually easier to interpret for this metric, in general.

## acidplots 0.2.3 (2019-05-05)

### Major changes

- Now pinned against R >= 3.5.

## acidplots 0.2.2 (2019-04-26)

### Major changes

- Now importing `upset` function from UpSetR, with modified formals, so we can
  easily call this from other packages, including [DESeqAnalysis][].

## acidplots 0.2.1 (2019-04-25)

### New functions

- New `gradient` palette functions: `purpleOrange`, `blueYellow`.
- Remove gplots package from imports.

### Major changes

- `plotHeatmap` now supports automatic color calculation when `breaks` argument
  is defined. Also improved support for `legendBreaks`.

### Minor changes

- Fix `.pheatmapColorPalette` internal check for `color`, which requires `n`
  argument, rather than assuming positional variable.
- `plotCountsPerFeature`: Improve the count subtitle.
- S4 generic reexport documentation fixes.
- Miscellaneous documentation fixes.

## acidplots 0.2.0 (2019-04-22)

Reworked package, consolidating previous minimalism and firestarter packages.

### New functions

- `synesthesia` color palette functions.
- `acid_coord_flip`: Intelligently puts samples at the top of the Y axis when
  flipped. Particularly useful for barplots.
- `acid_pretty_breaks`: Attempt to improve `scales::pretty_breaks`, using an
  approach more similar to the base plot engine.
- Renamed ggplot2 themes: `acid_theme_light`, `acid_theme_dark`.

### Major changes

- All ggplot2 geoms and transformations (`ggproto` functions) now consistently
  use `acid_` as a prefix.

### Minor changes

- `plotCountsPerBiotype` and `plotCountsPerBroadClass` now support `title`.

### Deprecations

- `plotCountsPerGene` has been renamed to `plotCountsPerFeature`.
- `plotGenesDetected` has been renamed to `plotFeaturesDetected`.
- Deprecated `theme_paperwhite` and `theme_midnight` in favor of
  `acid_theme_light` and `acid_theme_dark`.

## acidplots 0.1.2 (2019-04-18)

### New functions

- `base_breaks`: An attempt as automatic pretty breaks based on the default R
  plotting engine. Derived from a Stack Overflow post (see documentation).

### Minor changes

- `plotCountsPerBiotype`, `plotCountsPerBroadClass` switched to recommending
  log10 counts by default.

## acidplots 0.1.1 (2019-04-15)

Updated packages depencies and switched to Docker for Travis CI.

## acidplots 0.1.0 (2019-03-28)

Initial release, migrating ggplot2 functions from [basejump][] package.

[DESeqAnalysis]: https://deseqanalysis.acidgenomics.com/
[basejump]: https://basejump.acidgenomics.com/
[ggplot2]: https://ggplot2.tidyverse.org/
