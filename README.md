# acidplots

[![Travis CI build status](https://travis-ci.com/acidgenomics/acidplots.svg?branch=master)](https://travis-ci.com/acidgenomics/acidplots)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/ykti8ek8gj2i7g5r/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/acidplots/branch/master)
[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Functions for plotting genomic data.

## Installation

### [Bioconductor][] method

We recommend installing the package with [BiocManager][].

```r
if (!require("BiocManager")) {
    install.packages("BiocManager")
}
BiocManager::install("remotes")
BiocManager::install("acidgenomics/acidplots")
```

[Bioconductor]: https://bioconductor.org/
[BiocManager]: https://cran.r-project.org/package=BiocManager
[R]: https://www.r-project.org/
