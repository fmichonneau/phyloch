\name{draw.boxes}
\Rdversion{1.1}
\alias{box.clades}
\alias{box.tips}
% TITLE
\title{Plot Boxes around Clades in a Phylogeny}
% DESCRIPTION
\description{
These functions allow to draw boxes around clades/lineages in the plotting of a phylogenetic tree. 
}
% USAGE
\usage{
box.clades(phy, nodes, col, border = NA, shaded = NULL,
    text = NULL, textcol = "black", cex = 1, rshift = 0,
    align, expand = c(0, 0, 0, 0))
    
box.tips(phy, nodes, col, border = NA, 
    text = NULL, textcol = "black", cex = 1, rshift = 0, 
    align, expand = c(0, 0, 0, 0))
}

\arguments{
	
  \item{phy}{an object of class \code{\link[ape]{phylo}}.}

  \item{nodes}{a vector of mode \code{numeric} giving the MRCA node(s) of the clade(s) to be highlighted.}
  
  \item{col}{colors to fill the boxes with; default is \code{"lightgrey"}. Use \code{col = NA} to for non-filled, transparent boxes.}
  
  \item{border}{colors of the borders of boxes. Use \code{border = NA} to omit borders.}
  
  \item{shaded}{either \code{NULL} (then boxes will be filled with the color(s) specified with \code{col}) or a list of four \bold{named!} elements: \code{h} (hue), \code{c} (chroma), \code{l} (luminance), and \code{power} (increase of chroma and luminance to the power of ...). See \code{\link{hcl}} and \code{\link{sequential_hcl}}.}
  
  \item{text}{a character string giving the text to be plotted to the right of the clade. Can be set to \code{NULL} (the default) to omit plotting of text.}
    
  \item{cex}{numeric, character expansion, i.e. size, for text.}
  
  \item{textcol}{a vector of mode \code{character}, giving colors for the text.}
  
  \item{rshift}{a numeric used to shift the right boundary of the box further to the right, e.g. to place more text.}

  \item{align}{a vector with three possible values: \code{"tipwise"}, \code{"cladewise"}, or \code{"all"}, which controls the 'outer' (i.e distal to the root) alignment of boxes. \code{"tipwise"} is only allowed in \code{box.tips}. See Examples section for a demonstration. 	
 }
   
  \item{expand}{a numerical vector of the form \code{c(bottom, left, top, right)} which controls the size of boxes.}
  
  
  \item{...}{further graphical parameters may also be supplied as arguments.}   
}
% DETAILS
%\details{
%\code{col} argument. 
%}
% NOTE
\note{
Do not expect these functions to work with all flavors of \code{\link{plot.phylo}}: Currently, \code{box.clades} and \code{box.tips} only work with directions \code{"rightwards"}, \code{"leftwards"}, and \code{"upwards"} with plots of type \code{"phylogram"} and \code{"cladogram"}. The \code{text} argument works only with directions \code{"rightwards"} and \code{"leftwards"}.

\code{box.clades} and \code{box.tips} requires the use of \code{\link{plot.phylo.upon}} (see Examples section).
}
% AUTHOR
\author{Christoph Heibl}
% SEEALSO
\seealso{
\code{\link{plot.phylo}} for plotting of phylogenetic trees; \code{\link{clade.bars}} for plotting boxes around tips/clades; \code{\link{tip.color}} and \code{\link{edge.color}} for coloring tips labels or braches/edges of a plotted tree.
}
\examples{
## DATA: phylogentic relationships of bird orders:
## -----------------------------------------------
data(viperidae)
genera <- list("Montivipera", "Macrovipera", "Vipera", "Daboia")
genera.mrca <- noi(viperidae, regex = TRUE, genera)
cls <- c("yellow", "skyblue", "orange", "pink")

## EXAMPLE 1: the defaults of box.clades
## -------------------------------------
#plot(viperidae, edge.color = 0, tip.color = 0, no.margin = TRUE)
#box.clades(viperidae, genera.mrca)
#plot.phylo.upon(viperidae, no.margin = TRUE)

## EXAMPLE 2: the defaults of box.tips
## -------------------------------------
#plot(viperidae, edge.color = 0, tip.color = 0, no.margin = TRUE)
#box.tips(viperidae, genera.mrca)
#plot.phylo.upon(viperidae, no.margin = TRUE)
}
