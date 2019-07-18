# acidplots

[![Travis CI build status](https://travis-ci.com/acidgenomics/acidplots.svg?branch=master)](https://travis-ci.com/acidgenomics/acidplots)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/ykti8ek8gj2i7g5r/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/acidplots/branch/master)
[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Functions for plotting genomic data.

## Installation

### [R][] method

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
Sys.setenv(R_REMOTES_UPGRADE = "always")
# Set `GITHUB_PAT` in `~/.Renviron` if you get a rate limit error.
remotes::install_github("acidgenomics/acidplots")
remotes::update_packages()
```

[Bioconductor]: https://bioconductor.org/
[BiocManager]: https://cran.r-project.org/package=BiocManager
[R]: https://www.r-project.org/
