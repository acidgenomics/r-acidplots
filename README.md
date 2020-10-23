# AcidPlots

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/AcidPlots.svg?branch=master)](https://travis-ci.com/acidgenomics/AcidPlots)
[![Install with Bioconda](https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat)](http://bioconda.github.io/recipes/r-acidplots/README.html)

Functions for plotting genomic data.

## Installation

### [R][] method

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "AcidPlots",
    repos = c(
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    )
)
```

### [Conda][] method

Configure [Conda][] to use the [Bioconda][] channels.

```sh
# Don't install recipe into base environment.
name="r-acidplots"
conda create --name="$name" "$name"
conda activate "$name"
R
```

[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
[r]: https://www.r-project.org/
