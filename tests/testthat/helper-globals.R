data(
    rse, sce,
    package = "acidtest",
    envir = environment()
)

object <- rse
rownames <- head(rownames(rse))
g2s <- basejump::Gene2Symbol(rse)
geneIDs <- head(g2s[["geneID"]])
geneNames <- head(g2s[["geneName"]])
genes <- geneIDs
