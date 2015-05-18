\name{phyloch-package}
\alias{phyloch-package}
\alias{phyloch}
\docType{package}
\title{Phylogenetic Data Analysis And Display}
\description{
This package is intended as a (modest) companion to the great \code{ape} package, which it extends in two dimensions. First, it provides a series of wrappers for commonly used software packages and second, it contains some more tools for plotting of phylogentic trees.
}
\details{
\tabular{ll}{
 Package: \tab phyloch\cr
 Type: \tab Package\cr
 Version: \tab 1.5-5\cr
 Date: \tab 2013-09-04\cr
 License: \tab GPL (>= 2)\cr
}
There are several functions for reading and writing DNA sequences in FASTA, PHYLIP, and NEXUS format: \code{\link{read.fas}}, \code{\link{read.phy}}, \code{\link{read.nex}}, \code{\link{write.fas}}, \code{\link{write.phy}}, \code{\link{write.nex}}, and \code{\link{write.partitioned.nex}}.
Several functions are provided to extend the capabilities of \code{plot.phylo} from the ape package. Use \code{\link{clade.bars}} and \code{\link{box.clades}} to summarize clades and provide labels for clades in a phylogeny. \code{\link{tip.color}} and \code{\link{edge.color}} can be used in a simple way in conjunction with \code{plot.phylo} to create different color patterns for tips and edges.
Some functions are available for integrating BEAST with R. XML input files for BEAST can be generated with \code{\link{rbeauti}}. Two functions are designed to read TreeAnnotator output: \code{\link{read.beast}} will render an object of class \code{phylo} with additional node statistics appended as list elements. These additional node statistics will be lost be the subsequent use of \code{\link{ladderize}} or \code{\link{rotate}} (or similar functions that change the ordering of internal nodes); in this case \code{\link{node.trans}} can be used to reappend the nodestatistics.\code{\link{read.beast.table}} also parses the TreeAnnotator output, but returns a matrix of node statistics. Finally, HPD intervals can be plotted using \code{\link{HPDbars}}.
This package itself does not implement techniques for phylogenetic analyses, but provides a series of wrappers for commonly used software packages. Sequence alignment can be done with the \code{\link{mafft}} and \code{\link{prank}}; cleaning of sequences with \code{\link{gblocks}} and \code{\link{aliscore}}. The function \code{\link{raxml}} and \code{\link{mrbayes}} are intended for phylogentic tree search. Running \code{\link{mrbayes}} with argument \code{run = FALSE} can be used to create MrBayes-executable NEXUS files. Finally,  wrappers is provided for \code{Multistate} in the \code{BayesTraits} package (see \code{\link{multistateML}} and \code{\link{multistateMCMC}}).
}
\author{Natalie Cusimano, Christoph Heibl
Maintainer: Christoph Heibl (\email{christoph.heibl@gmx.net})
}
\keyword{package}
\seealso{
\code{\link[ape:ape-package]{ape}}
}
