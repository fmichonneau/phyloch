\name{extract.clade2}
\Rdversion{1.1}
\alias{extract.clade2}

\title{Extract Clades from a Phylogentic Tree}

\description{
As \code{extract.clades} from the ape package, \code{extract.clades2} keeps all the tips from a given node, and deletes all the other tips, but, in addition, can handle further, non-standard list elements of \code{"phylo"} object (i.e. vectors of length \code{phy$Nnode}, which may contain support values, ages or rate estimates, etc. ).
}

\usage{
extract.clade2(phy, node)
}

\arguments{
  \item{phy}{an object of class \code{"phylo"}
}
  \item{node}{an integer, giving the number of MRCA of the clade to be extracted
}
}

\value{
An object of class \code{"phylo"}.
}

\author{
Christoph Heiblbased on \code{extract.clade} by Emanuel Paradis.
}

\seealso{\code{\link{extract.clade}}, \code{\link{drop.tip2}}}
\examples{
	# To be added
}
