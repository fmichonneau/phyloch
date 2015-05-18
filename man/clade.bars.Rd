\name{clade.bars}
\Rdversion{1.1}
\alias{clade.bars}
% TITLE
\title{Plot Bars beside Clades in a Phylogeny}
% DESCRIPTION
\description{
This functions can be used to highlight clades/lineages in a plot of a phylogenetic tree with bars, possibly accompanied by additional text. 
}
% USAGE
\usage{
clade.bars(phy, nodes, bars = TRUE, barcol, barwd, 
    text = NULL, textcol = "black", srt, 
    cex = 1, offset, align, s = "clade", ...)
}
% ARGUMENTS
\arguments{
	
\item{phy}{
  an object of class \code{\link[ape]{phylo}}.
}
\item{nodes}{
  a vector of mode \code{numeric} giving the most recent common ancestor (MRCA) node of the clades to be highlighted. You can use \code{\link{noi}} to obtain this vector.
}
\item{bars}{
  \code{logical} indicating whether bars should be drawn.
}
\item{barcol}{
  a vector of mode \code{character} giving the color(s) of the bars; will be receycled if appropriate; default is \code{"grey"}.
}
\item{barwd}{
  a vector of mode \code{numeric} giving the width of bars; will be receycled if appropriate; default is \code{5}.
}
\item{text}{
  a string giving the text to be plotted to the right of the clade. Can be set to \code{NULL} (the default) to omit plotting of text.
}
\item{textcol}{
  a vector of mode \code{character}, giving the color(s) for the text; will be recycled if appropriate.
}
\item{srt}{
  numeric, the rotation of text in degrees.
}
\item{cex}{
  numeric \bold{c}haracter \bold{ex}pansion factor; multiplied by \code{\link{par}("cex")} yield the final character size for \code{text}.
}
\item{offset}{
  a vector of length one or two giving the offset for plotting bars and their labels. If two offset values are given, the first is used for the spacing between tip labels (or other previously plotted elements) and bars, and the second offsets the bar labels from the bars. \code{offset} might be of mode \code{"numeric"} or \code{"character"}; in the latter case the character string's width is determined by \code{\link{string.dim}} and the result is used for spacing.
}

\item{align}{a vector with two possible values that controls the alignment of bars: \code{"cladewise"} finds the position of bars for each clade separately (default), whereas \code{"all"} means that bars will be aligned according to the "most evolved" clade, i.e. the clade with the highest root-tip distance. 	
 }
\item{s}{
  a vector of mode \code{"character"}: By default \code{clade.bars} prints strings in the \code{text} argument in italics, except for those given by the \code{s} argument.
}
\item{...}{
  further graphical parameters may also be supplied as arguments.}   
}
% DETAILS
%\details{
%\code{col} argument. 
%}
% NOTE
\note{
Do not expect \code{clade.bars} to work with all flavors of \code{\link{plot.phylo}}: Currently, \code{clade.bars} works well only with types \code{"phylogram"} and \code{"cladogram"} in directions \code{"rightwards"}, \code{"leftwards"}, and \code{"upwards"}. Further extensions of the code are always welcome!
}
% AUTHOR
\author{Christoph Heibl}
% SEEALSO
\seealso{
\code{\link{plot.phylo}} for plotting of phylogenetic trees; \code{\link{box.tips}} and \code{\link{box.clades}} for plotting boxes around tips/clades; \code{\link{tip.color}} and \code{\link{edge.color}} for coloring tips labels or braches/edges in a plotted tree.
}
% EXAMPLES
\examples{
## DATA: phylogentic relationships of bird orders:
## -----------------------------------------------
data(viperidae)
genera <- list("Montivipera", "Macrovipera", "Vipera", "Daboia")
genera.mrca <- noi(viperidae, regex = TRUE, genera)
cls <- c("yellow", "skyblue", "orange", "pink")

## EXAMPLE 1: the defaults
## -----------------------
plot(viperidae)
clade.bars(viperidae, genera.mrca)
}
