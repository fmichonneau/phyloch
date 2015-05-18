\name{gradstein04}
\alias{gradstein04}
\docType{data}
\title{Stratigraphic Chart 2004}
\description{
The geological time scale of Gradstein \emph{et. al} (2004).
}
\usage{data(gradstein04)}
\format{
  A data frame containing the following columns:
  \describe{
    \item{\code{eon}}{Eon name.}
    \item{\code{era}}{Era name.}
    \item{\code{period}}{Period name.}
    \item{\code{epoch}}{Epoch name.}
    \item{\code{stage}}{Stage name.}
    \item{\code{MA}}{Estimated boundary age for the associated interval.}
    \item{\code{error}}{Estimated errors associated with each age estimate.}
    \item{\code{GSSP}}{Binary response denoting whether the age estimate is defined by a basal Global Standard Section and Point}
  }
}
\details{
Full details on derivation of the time scale are given in the references below. The following excerpt is the ad verbatim legend from the published figure: "Subdivisions of the global geologic record are formally defined by their lower boundary. Each unit of the Phanerozoic (~542 Ma to Present) and the base of Ediacaran are defined by a basal Global Standard Section and Point (GSSP), whereas Precambrian units are formally subdivided by absolute age (Global Standard Stratigraphic Age, GSSA). Details of each GSSP are posted on the ICS website \url{www.stratigraphy.org}.
Numerical ages of the unit boundaries in the Phanerozoic are subject to revision. Some stages within the Cambrian will be formally named upon international agreement on their GSSP limits. Most sub-Series boundaries (e.g., Middle and Upper Aptian) are not formally defined."
}
\source{
These data have been compiled by Thomas Ezard and Andy Purvis for the \code{paleoPhylo} package (\url{http://r-forge.r-project.org/projects/paleophylo}). 
}
\references{
Gradstein F.M., Ogg J.G., Smith A.G. 2004 A Geologic Time Scale 2004. Cambridge University Press, Cambridge, UK. \url{www.cambridge.org/uk/catalogue/catalogue.asp?isbn=0521786738}
Ogg, J.G., Ogg, G. and Gradstein, F.M. 2008 The Concise Geologic Time Scale. Cambridge University Press, Cambridge, UK. \url{www.cambridge.org/catalogue/catalogue.asp?isbn=0521898498}
Gradstein F.M. and Ogg J.G. Geologic Time Scale 2004: Why, how and where next! \url{www.stratigraphy.org/scale04.pdf}
}
\examples{
# phylogenetic relationships of bird orders:
# -----------------------------------------
data(bird.orders)
tr <- bird.orders

# Stratigraphic chart 2012
# ------------------------
data(gradstein04)

# Classic:
# --------
plot.phylo(tr, edge.width = 2, label.offset = 0.5)
axisGeo(GTS = gradstein04, unit = c("stage", "epoch", "period",
    "era"), cex = 0.8)

# You can impose a grid ...
# -------------------------
plot.phylo(tr, edge.width = 2, label.offset = 0.5)
axisGeo(GTS = gradstein04, unit = "stage", col = c("grey80",
    "white"), gridty = 3)

# ... or use colors:
# ------------------
plot.phylo(tr, edge.width = 2, label.offset = 0.5)
axisGeo(GTS = gradstein04, unit = "epoch", col = c("lightcyan",
    "lightpink"), texcol = "navy", gridty = 4, gridcol = "red")
  
# A tree with non-contemporaneous tips:
# -------------------------------------
# youngest tip:
yt <- 23.8

plot.phylo(tr, edge.width = 2, label.offset = 0.5)
axisGeo(gradstein04, tip.time = yt, unit = c("stage", "epoch"))

# The same tree with tips pointing upward:
# ---------------------------------------------
par(mar = c(2,5,2,2)) # adjust left-hand margin
plot.phylo(tr, edge.width = 2, direction = "upward")
axisGeo(gradstein04, tip.time = yt, unit = c("stage", "epoch"), 
  gridty = 3, gridcol = "red")
}
\keyword{datasets}

