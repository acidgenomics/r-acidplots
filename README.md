# minimalism

[![Travis CI build status](https://travis-ci.com/acidgenomics/minimalism.svg?branch=master)](https://travis-ci.com/acidgenomics/minimalism)
[![Build status](https://ci.appveyor.com/api/projects/status/ykti8ek8gj2i7g5r/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/minimalism/branch/master)
[![Codecov percent coverage](https://codecov.io/gh/acidgenomics/minimalism/branch/master/graph/badge.svg)](https://codecov.io/gh/acidgenomics/minimalism)
[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Minimalist visualizations for bioinformatics.

## Installation

### [Bioconductor][] method

We recommend installing the package with [BiocManager][].

```r
if (!require("BiocManager")) {
    install.packages("BiocManager")
}
BiocManager::install("remotes")
BiocManager::install("acidgenomics/minimalism")
```

[Bioconductor]: https://bioconductor.org/
[BiocManager]: https://cran.r-project.org/package=BiocManager
[R]: https://www.r-project.org/
