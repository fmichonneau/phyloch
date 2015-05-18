\name{plotNetwork}
\alias{plotNetwork}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Haplotype/Genotype Networks}

\description{
This function is intented to plot reticulate phylogenetic data (e.g. hyplotype ot genotype networks, "minimum spanning trees"). It is in an eraly state of development - use with care!
}
\usage{
plotNetwork(m, freq, spread = 180, rotate = 0, mirror = FALSE, 
    freqmode = 2, loc = NULL, locorder = "lat", cex = 1, 
    col = "r", piecolrev = FALSE, contrast = 0, border = NA, 
    xshift = 0, yshift = 0, replace.names = FALSE, stp = NA, 
    modify = NULL)
}

\arguments{
  \item{m}{A binary matrix of node connection: 1 means there is an edge between node[m] and node[n]; 0 means no edge.
}
  \item{freq}{A named vector of haplotype frequencies; if omitted the frequency of each haplotype is taken to be 1.}
  \item{spread}{
%%     ~~Describe \code{spread} here~~
}
  \item{rotate}{A numeric vector of length one: the angle of clock-wise rotation of the whole plot.
}
  \item{mirror}{Logical: if \code{TRUE}, the whole plot is mirrored through X-axis.
}
  \item{freqmode}{
\emph{Currently not implemented.}
}
  \item{loc}{
\emph{Currently not implemented.}
}
  \item{locorder}{
\emph{Currently not implemented.}
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{piecolrev}{
%%     ~~Describe \code{piecolrev} here~~
}
  \item{contrast}{
%%     ~~Describe \code{contrast} here~~
}
  \item{border}{
%%     ~~Describe \code{border} here~~
}
  \item{xshift}{
%%     ~~Describe \code{xshift} here~~
}
  \item{yshift}{
%%     ~~Describe \code{yshift} here~~
}
  \item{replace.names}{Logical: if \code{TRUE}, the names of the haplotypes are replaced by consecutive capital letters.
}
  \item{stp}{
\emph{Currently not implemented.}
}

  \item{modify}{
\emph{Currently not implemented.}
}
}

\value{
\emph{Currently no value implemented.}
}

\author{
Christoph Heibl
}

\seealso{
\code{\link{mst}}, \code{\link{plot.mst}}
}
\examples{
data(woodmouse)
mst <- mst(dist.dna(woodmouse))
plotNetwork(mst, mirror = TRUE)
plotNetwork(mst, replace.names = TRUE)

# include information about frequency of haplotypes
# -------------------------------------------------
freq <- c(1, 1, 7, 2, 2, 3, 3, 1, 1, 4, 1, 1, 1, 1, 2)
names(freq) <- rownames(mst)
plotNetwork(mst, freq = freq, replace.names = TRUE)
}