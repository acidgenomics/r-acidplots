% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/plotReducedDim-methods.R
\name{plotPca}
\alias{plotPca}
\alias{plotReducedDim}
\alias{plotTsne}
\alias{plotUmap}
\alias{plotReducedDim,SingleCellExperiment-method}
\alias{plotPca,SingleCellExperiment-method}
\alias{plotPca,SummarizedExperiment-method}
\alias{plotTsne,SingleCellExperiment-method}
\alias{plotUmap,SingleCellExperiment-method}
\title{Plot reduced dimensions}
\usage{
plotPca(object, ...)

plotReducedDim(object, ...)

plotTsne(object, ...)

plotUmap(object, ...)

\S4method{plotReducedDim}{SingleCellExperiment}(
  object,
  reduction = "UMAP",
  dims = 1:2,
  interestingGroups = NULL,
  color = getOption(x = "acid.discrete.color", default =
    AcidPlots::acid_scale_color_synesthesia_d()),
  pointSize = getOption(x = "acid.point.size", default = 1L),
  pointAlpha = getOption(x = "acid.point.alpha", default = 0.9),
  pointsAsNumbers = getOption(x = "acid.points.as.numbers", default = FALSE),
  label = getOption(x = "acid.label", default = FALSE),
  labelSize = getOption(x = "acid.label.size", default = 6L),
  dark = getOption(x = "acid.dark", default = FALSE),
  legend = getOption(x = "acid.legend", default = TRUE),
  labels = list(title = NULL, subtitle = NULL)
)

\S4method{plotPca}{SingleCellExperiment}(object, ...)

\S4method{plotPca}{SummarizedExperiment}(
  object,
  assay = 1L,
  interestingGroups = NULL,
  ntop = 500L,
  label = getOption(x = "acid.label", default = FALSE),
  pointSize = getOption(x = "acid.point.size", default = 3L),
  labels = list(title = "PCA", subtitle = NULL)
)

\S4method{plotTsne}{SingleCellExperiment}(object, ...)

\S4method{plotUmap}{SingleCellExperiment}(object, ...)
}
\arguments{
\item{object}{Object.}

\item{reduction}{\code{vector(1)}.
Dimension reduction name or index position.}

\item{dims}{\code{integer}.
Vector of length 2 that denotes the columns from the reduced dimension
matrix to use for \code{centerX} and \code{centerY} column calculations. Defaults the
first and second dimensions.}

\item{interestingGroups}{\code{character}.
Groups of interest to use for visualization.
Corresponds to factors describing the columns of the object.}

\item{color}{\code{ScaleDiscrete}.
Desired ggplot2 color scale. Must supply discrete values. When set \code{NULL},
the default ggplot2 color palette will be used. If manual color definitions
are desired, we recommend using \code{ggplot2::scale_color_manual()}.

To set the discrete color palette globally, use:

\if{html}{\out{<div class="sourceCode">}}\preformatted{options("acid.color.discrete" = ggplot2::scale_color_viridis_d())
}\if{html}{\out{</div>}}}

\item{pointSize}{\code{numeric(1)}.
Point size for dots in the plot.
In the range of 1-3 is generally recommended.}

\item{pointAlpha}{\code{numeric(1)} (\code{0}-\code{1}).
Alpha transparency level.}

\item{pointsAsNumbers}{\code{logical(1)}.
Plot the points as numbers (\code{TRUE}) or dots (\code{FALSE}).}

\item{label}{\code{logical(1)}.
Superimpose sample text labels on the plot.}

\item{labelSize}{\code{integer(1)}.
Size of the text label.}

\item{dark}{\code{logical(1)}.
Plot against a dark background using the \code{acid_theme_light()} ggplot2
theme.}

\item{legend}{\code{logical(1)}.
Include plot legend.}

\item{labels}{\code{list}.
ggplot2 labels.
See \code{ggplot2::labs()} for details.}

\item{...}{Additional arguments.}

\item{assay}{\code{vector(1)}.
Assay name or index position.}

\item{ntop}{\code{integer(1)} or \code{Inf}.
Number of most variable genes to plot.
Use \code{Inf} to include all genes (\emph{not recommended}).}
}
\value{
\code{ggplot}.
}
\description{
Plot cell-level dimensionality reduction.
}
\details{
For \code{SingleCellExperiment}, colors using \code{ident} column defined in
\code{colData()} by default.
}
\note{
Updated 2022-03-07.
}
\section{Reduction types}{

\itemize{
\item PCA: \strong{P}rincipal \strong{C}omponent \strong{A}nalysis.
\item t-SNE: \strong{t}-distributed \strong{S}tochastic \strong{N}eighbor \strong{E}mbedding.
\item UMAP: \strong{U}niform \strong{M}anifold \strong{A}pproximation and \strong{P}rojection.
}
}

\section{Principal component analysis (\code{plotPca})}{


PCA (Jolliffe, et al., 2002) is a multivariate technique that allows us to
summarize the systematic patterns of variations in the data. PCA takes the
expression levels for genes and transforms it in principal component space,
reducing each sample into one point. Thereby, we can separate samples by
expression variation, and identify potential sample outliers. The PCA plot is
a way to look at how samples are clustering.

We're using a modified version of the \code{DESeqTransform} method here.
}

\section{UMAP calculation}{


\href{https://github.com/lmcinnes/umap}{UMAP} calculation in R requires the \href{https://www.python.org}{Python} module \code{umap-learn}.
The UMAP module can be loaded in R using \href{https://rstudio.github.io/reticulate/}{reticulate}.
}

\examples{
data(
    RangedSummarizedExperiment,
    SingleCellExperiment_Seurat,
    package = "AcidTest"
)

## SummarizedExperiment ====
object <- RangedSummarizedExperiment
plotPca(object)

## SingleCellExperiment ====
object <- SingleCellExperiment_Seurat
plotReducedDim(object, reduction = "UMAP")
}
\references{
Jolliffe, et al., 2002.
}
\seealso{
\itemize{
\item \code{DESeq2::plotPca()}.
\item \code{Seurat::DimPlot()}.
\item \code{monocle3::plot_cells()}.
}
}
\author{
Michael Steinbaugh, Rory Kirchner
}
