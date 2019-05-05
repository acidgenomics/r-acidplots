#!/usr/bin/env Rscript

Sys.setenv(TZ = "America/New_York")
options(
    deparse.max.lines = 3L,
    error = quote(rlang::entrace()),
    rlang_backtrace_on_error = "full",
    showErrorCalls = TRUE,
    showWarnCalls = TRUE,
    warn = 1L,
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE,
    warning.length = 8170L
)

sessioninfo::session_info()

rcmdcheck::rcmdcheck(
    path = ".",
    args = c(
        "--no-build-vignettes",
        "--no-manual",
        "--no-vignettes",
        "--timings"
    ),
    build_args = c(
        "--no-build-vignettes",
        "--no-manual"
    ),
    error_on = "error"
)

if (packageVersion("base") >= "3.6") {
    BiocCheck::BiocCheck(
        package = ".",
        `quit-with-status` = TRUE
    )
    lintr::lint_package()
}
