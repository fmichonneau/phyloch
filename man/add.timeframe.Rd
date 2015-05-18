\name{add.timeframe}
\Rdversion{1.1}
\alias{add.timeframe}

\title{High-lighting ages in chronograms}

\description{This function ...}

\usage{
add.timeframe(phy, age, clade = NULL, tips = NULL, ...)
}
 
\arguments{
  \item{phy}{An object of class \code{phylo}.}
  
  \item{age}{A vector of mode \code{"numeric"} giving an age or an age range.}
  
  \item{clade}{An integer giving the the node number of the MRCA of a clade to which the timeframe should be restricted.}
  
  \item{tips}{A vector of integers giving the numbers of tips to which the time frame should be restricted. Can be used for non-monophyletic groups.}
  
  \item{\dots}{Further arguments.}
}

\value{none}

\author{Christoph Heibl}

\seealso{\code{\link{add.geoscale}}, \code{\link{read.beast}}, \code{\link{noi}}}

\examples{	
# phylogentic relationships of bird orders:
# -----------------------------------------
data(bird.orders)

# plot tree:
# setting edge and tip colors to zero (invisible) improves
# resolution 
# ----------
plot(bird.orders, edge.color = 0, tip.color = 0)

# highlight some geological event, etc. ...
# -----------------------------------------
add.timeframe(bird.orders, age = c(12, 14), col = "skyblue",
	border = NA)

# restrict to certain clade ...
# -----------------------------
cl <- noi(bird.orders, c("Struthioniformes", "Anseriformes"))
add.timeframe(bird.orders, age = c(16:21), col = "pink",
	border = NA, clade = cl)

# or to some other non-monophyletic assemby:
# ------------------------------------------
add.timeframe(bird.orders, age = c(17:24), col = "yellow",
	border = NA, tips = 6:8)

# now plot tree again:
# arguments must be exactly the same of course, except for
# edge and tip colors
# -------------------
plot.phylo.upon(bird.orders)
	
}