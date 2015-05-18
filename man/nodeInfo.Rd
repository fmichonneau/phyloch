\name{nodeInfo}
\alias{nodeInfo}

\title{
Graphical Annotation of Internal Nodes
}
\description{
This function can be used to graphically highlight internal nodes in a phylogentic tree. A character string giving attributes of the node can also be drawn as a framed textbox.
}
\usage{
nodeInfo(node, label, label2 = NULL, cex = 1, angle = 225, 
    col = "red", lex = 1)
}
\arguments{
  \item{node}{
An integer giving the number of an internal node.
}
  \item{label}{
%%     ~~Describe \code{label} here~~
}
\item{label2}{
%%     ~~Describe \code{label} here~~
}
  \item{cex}{
Numeric: \bold{c}haracter \bold{ex}pansion factor; multiplied by \code{\link{par}}("cex") yields the final character size. \code{NULL} and \code{NA} are equivalent to \code{1.0}.
}
  \item{angle}{
Numeric: the angle in degree between 0 and 360, counted anti-clock-wise from ...
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lex}{
Numeric: \bold{l}ength \bold{ex}pansion factor.
}
}

\value{
None.
}

\author{Christoph Heibl}

\seealso{
\code{\link{clade.bars}}, \code{\link{box.clades}}
}
\examples{
# load a small phylogeny ...
data(vipera)

# and plot it.
plot(vipera.bayes)

# Find a certain internal node ...
p <- noi(vipera.bayes, c("Vipera_dinniki", "Vipera_berus"))

# ... which should be bear the following text ...
txt <- expression(paste("subgenus ", italic(Pelias)))

# and plot the information:
nodeInfo(p, txt, angle = 140, cex = 0.75, lex = 3)

# The position of the text can also be adjusted manually by 
# clicking on the plot:
nodeInfo(15, "It's easy!", angle = NULL, cex = 2, col = "blue")
}
