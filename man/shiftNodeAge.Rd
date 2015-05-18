\name{shiftNodeAge}
\alias{shiftNodeAge}

\title{Modify Node Ages in Ultramtric Trees}

\description{
The functions is intended to manually change the height of internal nodes (i.e. changing the 'age of nodes'), e.g. to create a tree prior for Bayesian phylogeny estimation (BEAST, etc.).
}
\usage{
shiftNodeAge(phy, node, shift = NULL, age = NULL)
}

\arguments{
  \item{phy}{
An object of class \code{"phylo"}. Must be ultrametric (see \code{\link{is.ultrametric}})
}
  \item{node}{
An integer, the number of the internal node whose age is to be modified.
}
  \item{shift}{
A real number giving the age shift of \code{node} in the same units as the branch lengths od \code{phy}. Positive numbers shift the node back in time; negative numbers render it younger. \code{shift} is overridden by \code{age}.
}
  \item{age}{
A real number giving the new age of \code{node} in the same units as the branch lengths of \code{phy}. Overrides \code{shift}.
}
}

\value{
An object of class \code{"phylo"}
}

\author{Christoph Heibl}

\seealso{
\code{\link{rbeauti}}, \code{\link{compare.phylo}}
}

\examples{
# Phylogeny of bird orders:
data(bird.orders)

# Have a look at the phylogeny:
plot(bird.orders)
axisPhylo()

# Change age of node 42 ...
tr <- shiftNodeAge(bird.orders, 42, -15)

# ... plot again:
plot(tr)
axisPhylo()

# Alternatively, the result can be checked with 'compare.phylo':
compare.phylo(bird.orders, tr)

# Note that time shifts must not exceed the available 
# branch length:
#tr <- shiftNodeAge(bird.orders, 42, +15)
}