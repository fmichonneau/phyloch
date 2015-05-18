\name{taxo.fonts}
\alias{taxo.fonts}

\title{Font Types for Tip Labels}

\description{This function can be used to render scientific names taxonomically correctly when plotting phylogenies.
}

\usage{
taxo.fonts(phy, cex, offset, col, align = FALSE, auto.correction = 
    TRUE, s = c("spec.", "subspec.", "var.", "cf.", "agg.",
    "syn."), lty = 3, lcol = "grey25")
}

\arguments{
  \item{phy}{An object of class \code{phylo}.
}
  \item{cex}{A real number, giving the \bold{c}haracter \bold{ex}pension (see \code{\link{par}}).
}
\item{offset}{A real number giving the horizontal offset between tips and tip labels.
}
\item{col}{A character string, giving the color(s) to be used for the tip labels).
}
\item{align}{Logical, if \code{TRUE}, the tip labels will be left-justified according to the largest root-tip distance + offset.
}
   \item{auto.correction}{Logical; if \code{TRUE}, tip labels will be corrected for non-official abbreviations. 
}
  \item{s}{A vector of mode \code{character} containing the strings that will be printed in plain font type.
}
\item{lty}{A numeric, giving the line type if \code{align = TRUE}. The plotting of the lines can be suppressed with \code{lty = 0}. See \code{\link{par}} for more options.
}
\item{lcol}{A character string, giving the line color if \code{align = TRUE}.}
}

\details{
Use \code{\link{plot.phylo}} with the \code{tip.color} argument set to \code{0} to suppress plotting of the tip labels. This gets you the correct extension of the plotting region, which would not be the case with \code{show.tip.label = FALSE}.
}

\value{
None.
}

\author{Christoph Heibl}

\seealso{\code{\link{plot.phylo}}}
\examples{
# molecular phylogeny of Eurasian vipers:
# ---------------------------------------
data(viperidae)

# plot phylogeny: strings contained in 's' will be 
# in plain font type and the corresponding tips labels 
# colored brown; note the use of 'tip.color = 0' in 
# 'plot.phylo'
# ----------------------------------------------------
s  <-  c("ssp.", "var.", "spec.", "cf.", "agg.", "syn.")
tcol <- tip.color(viperidae, s, col = "brown") 
plot.phylo(viperidae, tip.color = 0)
taxo.fonts(viperidae, col = tcol)

# align tip labels:
# here we need to set the x.lim argument in plot.phylo
# ----------------------------------------------------
plot.phylo(viperidae, tip.color = 0, x.lim = c(0, 1.3)) 
taxo.fonts(viperidae, align = TRUE, offset = 0.1)
}
