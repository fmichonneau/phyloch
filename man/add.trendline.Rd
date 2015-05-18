\name{add.trendline}
\Rdversion{1.1}
\alias{add.trendline}

\title{Plotting Trendlines on a Phylogeny}

\description{This function can be used to add a trendline for some given variable to an ultrametric phylogenetic tree.}

\usage{
add.trendline(phy, x, axis = TRUE, ...)
}
 
\arguments{
	
  \item{phy}{An object of class \code{phylo}.}
  
  \item{x}{A matrix, containing data points: x-values (ages) in the first column, y-values in the second columns.}
  
  \item{axis}{A logical indicating if a y-axis should be plotted.}
  
  \item{...}{Further arguments to be passed to \code{\link{lines}}.}
}

\value{none}

\author{Christoph Heibl}

\seealso{\code{\link{add.timeframe}}}

\examples{
# phylogenetic tree of wild barley
# --------------------------------
data(hordeum)

# plot tree
# ---------
plot(hordeum)

# use some dummy data ...
# -----------------------
x <- seq(from = 12, to = 0, length.out = 15)
y <- c(1, 2, 4, 4, 6, 5, 11, 13, 15, 16, 12, 14, 15, 17, 18)
data <- cbind(x, y)

# ... to plot trendline:
# ----------------------
add.trendline(hordeum, x = data, col = "red", lty = 2)
axisChrono()
}
