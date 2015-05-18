\name{add.geoscale}
\Rdversion{1.1}
\alias{add.geoscale}

\title{Geological Time Scale for Phylogenies}

\description{
This function can be used to underlie chronograms with a geological time scale.
}

\usage{
add.geoscale(phy, alpha = 0.5, ...)
}

\arguments{
  \item{phy}{An object of class \code{phylo}.}
  
  \item{alpha}{The alpha transparency, a number in [0,1].}
  
  \item{\dots}{\emph{Do not use!}}
}

\value{none}

\author{Christoph Heibl}

\references{
International Commission on Stratigraphy. 2009. International stratigraphic chart: \url{http://www.stratigraphy.org/upload/ISChart2009.pdf}.
}

\examples{
	
# phylogentic relationships of bird orders:
data(bird.orders)

# plot tree:
# setting edge and tip colors to zero (invisible) improves
# resolution 
plot(bird.orders, edge.color = 0, tip.color = 0)

# underly chronogram with geological time scale
add.geoscale(bird.orders)

# now plot tree again:
# arguments must be exactly the same of course, except for
# edge and tip colors
plot.phylo.upon(bird.orders)
}
