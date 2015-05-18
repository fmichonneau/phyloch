\name{axisGeo}
\alias{axisGeo}

\title{
Geological Time Axis for Time-Calibrated Phylogenies
}
\description{
This function adds a geological time scale on the side of a phylogeny plot.
}
\usage{
axisGeo(GTS, tip.time = 0, unit = c("epoch", "period"), ages = 
    TRUE, cex = 1, col = "white", texcol = "black", gridty = 0, 
    gridcol = "black")
}

\arguments{
  \item{GTS}{a data frame containing the geological time scale such as provided by \code{\link{strat2012}}.}
  
  \item{tip.time}{a real number giving time units back from present were the youngest taxa were sampled.}

  \item{unit}{a vector of mode character used to select geological time units that shall be displayed. When using \code{gradstein04}, \code{"eon"}, \code{"era"}, \code{"period"}, \code{"epoch"}, and \code{"stage"} are available.}
  
  \item{ages}{logical: if \code{TRUE}, a real-numbered axis of unit bounderies is added to the plot.}
  	
  \item{cex}{a real number giving the \bold{c}haracter \bold{ex}pansion.}
  
  \item{col}{a vector of mode character used to select background color(s). Will be recycled if necessary.}
  
  \item{texcol}{a vector of mode character used to select font color(s). Will be recycled if necessary.}
  
  \item{gridty}{an integer representing the line type of the grid exactly in the same way as \code{lty} argument in \code{\link{par}}: 0 or \code{blank}, 1 or \code{"solid"}, 2 or \code{"dashed"}, 3 or \code{"dotted"}, 4 or \code{"dotdash"}, 5 or \code{"longdash"}, and 6 or \code{"twodash"}.}
  
  \item{gridcol}{a vector of mode character used to select grid color(s). Will be recycled if necessary.}

}
\details{
This function is intended to make the geological time scale from the \code{paleoPhylo} package available to users of the \code{ape} package (paleoPhylo is not yet on CRAN). For more functions for analysing and plotting paleontological data, see the \code{paleoPhylo} package on R-Forge.

If the name of a time interval does not fit the space on the plotting device provided by that time frame, \code{axisGeo} tries to fit the name string by lowering the original character expansion. In order to avoid undiscernable small font sizes only character expansion of 50 percent or more of the original \code{cex} is allowed; in all other cases strings are abreviated and plotted with the original character expansion. 
}

\author{
Christoph Heibl
}

\references{
Gradstein F.M., J.G. Ogg & A.G. Smith. 2004. A Geologic Time Scale. Cambridge University Press, Cambridge, UK. \url{www.cambridge.org/uk/catalogue/catalogue.asp?isbn=0521786738}.

International Commission on Stratigraphy. 2012. International stratigraphic chart: \url{http://www.stratigraphy.org/upload/ISChart2009.pdf}.
}

\seealso{\code{\link{strat2012}} for the current stratigraphic chart; \code{gradstein04} (the 2004 stratigraphic chart from the paleoPhylo package) is still provided for backward comparison. \code{\link{axisPhylo}}, \code{\link{axisChrono}}, \code{\link{add.geoscale}} are further possibilities add axes to plotted phylogenies.
}
\examples{
# phylogenetic relationships of bird orders:
# -----------------------------------------
data(bird.orders)
tr <- bird.orders

# Stratigraphic chart 2012
# ------------------------
data(strat2012)

# Classic:
# --------
plot.phylo(tr, edge.width = 2, label.offset = 0.5)
axisGeo(GTS = strat2012, unit = c("stage", "epoch", "period",
    "era"), cex = 0.8)

# You can impose a grid ...
# -------------------------
plot.phylo(tr, edge.width = 2, label.offset = 0.5)
axisGeo(GTS = strat2012, unit = "stage", col = c("grey80",
    "white"), gridty = 3)

# ... or use colors:
# ------------------
plot.phylo(tr, edge.width = 2, label.offset = 0.5)
axisGeo(GTS = strat2012, unit = "epoch", col = c("lightcyan",
    "lightpink"), texcol = "navy", gridty = 4, gridcol = "red")
	
# A tree with non-contemporaneous tips:
# -------------------------------------
# youngest tip:
yt <- 23.8

plot.phylo(tr, edge.width = 2, label.offset = 0.5)
axisGeo(strat2012, tip.time = yt, unit = c("stage", "epoch"))

# The same tree with tips pointing upward:
# ---------------------------------------------
par(mar = c(2,5,2,2)) # adjust left-hand margin
plot.phylo(tr, edge.width = 2, direction = "upward")
axisGeo(strat2012, tip.time = yt, unit = c("stage", "epoch"), 
	gridty = 3, gridcol = "red")
}
