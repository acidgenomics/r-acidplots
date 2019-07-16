#!/usr/bin/env Rscript

# Check package coverage with covr.
# Updated 2019-07-15.

options(
    error = quote(quit(status = 1L)),
    warning = quote(quit(status = 1L))
)

if (packageVersion("base") < "3.6") {
    quit()
}

requireNamespace("covr", quietly = TRUE)

cov <- covr::package_coverage()
pct <- covr::percent_coverage(cov)

if (pct < 100L) {
    print(cov)
    stop(sprintf("Coverage is %s.", round(pct, digits = 2L)))
}
