\name{vipera}
\alias{vipera}
\alias{vipera.bayes}
\alias{vipera.ml}
\alias{viperidae}
\alias{origin}

\title{Phylogenetic Trees of True Vipers}

\description{
  This is a 16S gene tree of Vipera which is extracted from a dataset of true vipers (Lenk et. al, 2001).
}

\usage{
data(vipera)

data(viperidae)
}

\format{
  The data are stored as an object of class \code{"phylo"}, see
  \code{read.tree} in the \code{ape} package.
}

\source{
  Lenk, P., S. Kalyabina, , M. Wink, and U. Joger (1990) Evolutionary relationships among the true vipers (Reptilia; Viperidae) inferred from mitochondrial DNA sequences. \emph{Mol. Phyl. Evol.} \bold{19}: 94-104.
}

\examples{
data(viperidae)
plot(viperidae)
}
\keyword{datasets}
