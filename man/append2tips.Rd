\name{append2tips}
\alias{append2tips}

\title{Append Symbols or Text to Tip Labels}

\description{
This function allows to add text or symbols to the tip labels of a phylogenetic tree
}
\usage{
append2tips(phy, tips, offset, align = FALSE,
	grid = FALSE, col = "red", text = NULL, pch = NULL, cex, ...)
}

\arguments{
  \item{phy}{an object of class \code{\link[ape]{phylo}}.
}
  \item{tips}{a vector of mode \code{"numeric"}, giving the tip numbers to which append text or symbols. If missing, all tips are used.
}
 
  \item{offset}{a real, used to control the horizontal offset of text or symbols from the tip labels of the phylogenetic tree.
}
  \item{align}{logical, if \code{TRUE} text or symbols are horizontal aligned.}
  
  \item{grid}{logial, if \code{TRUE} dotted lines are plotted as a horizontal grid to aid visual orientation.}

  \item{col}{a character string, setting the color for text of symbols
}

  \item{text}{a character string, giving the text to be appended to the tip labels
}
  \item{pch}{an integer, specifying the symbol used (see \code{\link{par}}).
}
  \item{cex}{a real number, giving the \bold{c}haracter \bold{ex}pension (see \code{\link{par}}). If missing, \code{cex} is taking from the previous call of \code{\link[ape]{plot.phylo}}.
}
 
  \item{\dots}{further graphical parameters (from par), such as srt, family and xpd.
}
}
\details{
Currently, \code{append2tips} is will work only for \code{\link[ape]{plot.phylo}} using types \code{"phylogram"} or \code{"cladogram"} and directions \code{"rightwards"} or \code{"leftwards"}.
}

\value{
None.
}

\author{Christoph Heibl}

\seealso{\code{\link{plot.phylo}}, \code{\link{tiplabels}}, \code{\link{taxo.fonts}}}
\examples{
# phylogeny of true vipers
# ------------------------
data(viperidae)
plot.phylo(viperidae, cex = 0.8, label.offset = 0.02, x.lim = 1.5)
NTips <- length(viperidae$tip.label)
append2tips(viperidae, text = 
	paste("A", 1:NTips, sep = "-"), col = "orange")
append2tips(viperidae, tips <- c(5, 7:9, 14), 
	text = rep("xxx", 5), col = "slateblue")
append2tips(viperidae, pch = 23, col = rainbow(NTips))
append2tips(viperidae, pch = 21, col = "grey50", 	
    align = TRUE, grid = TRUE)

}