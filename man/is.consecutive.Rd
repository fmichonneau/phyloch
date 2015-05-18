\name{is.consecutive}
\alias{is.consecutive}
\title{
Is Node Numbering Consecutive?
}
\description{
This function tests whether the numbering (indexing) of terminal nodes ("tips"") of a given tree is consecutive.
}
\usage{
is.consecutive(phy)
}
\arguments{
  \item{phy}{
a phylogentic tree of class \code{"phylo"}
}
}
\details{
In \code{ape} when doing tree rearrangements (e.g. \code{\link{root}} and \code{\link{ladderize}}) internal and terminal nodes will keep their number (index) and the consecutive numbering of terminal and internal nodes will be disrupted. This behavior might be problematic if the elements of vectors or lists are not matched to the actual node numbers but to the expected ("canonical") node numbers. \code{\link{is.consecutive}} allows to test for the consecutivity of node numbers.  
%% MrBayes trees are not consecutive
}
\value{
\code{TRUE} or \code{FALSE}
}
\author{
Christoph Heibl
}
\seealso{
\code{\link{fixNodes}} to enforce consecutive node numering on a phylogenetic tree; \code{\link{is.monophyletic}} and \code{\link{is.binary}} to explore tree structure.
}
\examples{
phy <- rtree(20)
is.consecutive(phy)
is.consecutive(ladderize(phy))
}
