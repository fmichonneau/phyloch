\name{axisChrono}
\alias{axisChrono}

\title{
Axis for time-calibrated phylogenies
}
\description{
This function adds a time scale on the side of a phylogeny plot.
}
\usage{
axisChrono(side = 1, unit = "Ma", fact = 1, ...)
}

\arguments{
  \item{side}{
A numeric value specifying the side where the axis is plotted: 1: below, 2: left, 3: above, 4: right.
}
  \item{unit}{
A character string used to provide information on time units; defaults to "Ma" (million years ago).
}
  \item{fact}{A real number used to scale the axis. }
  \item{\dots}{
Further arguments passed to \code{\link{axis}}.
}
}

\author{Christoph Heibl}

\seealso{\code{\link{axisGeo}}, \code{\link{add.geoscale}}
}
\examples{
# phylogentic relationships of bird orders:
data(bird.orders)

# plot tree:
plot(bird.orders)

# plot axis in units of million years ago
axisChrono(side = 1, unit = "Ma")
}