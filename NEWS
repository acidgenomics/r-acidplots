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
