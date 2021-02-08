suppressPackageStartupMessages({
    library(AcidGenomes)
})

list <- list()

## Homo sapiens ====
genes <- c(
    "ENSG00000012817",
    "ENSG00000067048",
    "ENSG00000114374",
    "ENSG00000173674",
    "ENSG00000183878",
    "ENSG00000229807"
)
gr <- makeGRangesFromEnsembl(
    organism = "Homo sapiens",
    level = "genes",
    ignoreVersion = TRUE
)
gr <- gr[genes]
gr <- droplevels(gr)
homoSapiens <- gr

## Mus musculus ====
genes <- c(
    "ENSMUSG00000056673",
    "ENSMUSG00000068457",
    "ENSMUSG00000069045",
    "ENSMUSG00000086503"
)
gr <- makeGRangesFromEnsembl(
    organism = "Mus musculus",
    level = "genes",
    ignoreVersion = TRUE
)
gr <- gr[genes]
gr <- droplevels(gr)
musMusculus <- gr

genderMarkers <- list(
    "homoSapiens" = homoSapiens,
    "musMusculus" = musMusculus
)
use_data(genderMarkers)
