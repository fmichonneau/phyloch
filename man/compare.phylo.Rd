\name{compare.phylo}
\alias{compare.phylo}

\title{Compare PHYLO Objects Graphically}

\description{
This function is intented for a graphical comparison of phylogenetic trees stored as objects of class \code{phylo}. First, trees are pruned to those terminal nodes, which are shared by both trees. Second, the first tree is displayed on the current graphical device and the presence of each of its clades in the second tree is displayed as a colored internal nodelabel. Finally, if trees are ultrametric, time shift of internal nodes present in both phylogenies will be shown by colored arrows.
}
\usage{
compare.phylo(x, y, ts = "arrow", threshold, presCol = "green", 
     absCol = "red", tsCol = "blue", pch, pcex, tipcex, plottype, ...)
}

\arguments{
  \item{x}{An object of class \code{phylo}.}
  
  \item{y}{An object of class \code{phylo} to be compared to \code{x}.}
  
  \item{ts}{Either \code{"arrow"} or \code{"numbers"}. In the first case time shift of nodes is depicted by arrows, in the second case the amount and direction of the time shift is printed as a real number onto the internal nodes.}
  
  \item{threshold}{Numeric: \code{threshold} is the fraction of the root age below which node time shift are \bold{not} plotted; defaults to 1 percent of the age of the root.}
  
  \item{presCol}{A character string giving the coloring for internal nodes \bold{pres}ent in both trees; defaults to \code{"green"}.}
  	
  \item{absCol}{A character string giving the coloring for internal nodes present in \code{x}, but \bold{abs}ent in \code{y}; defaults to \code{"red"}.}
  
  \item{tsCol}{A character string giving color for arrows displaying the \bold{t}ime \bold{s}hift of internal nodes.; defaults to \code{"blue"}.}
  
  \item{pch}{Either an integer specifying a symbol or a single character defining the shape of the internal node labels. See \code{\link{points}} for possible values and their interpretation. Note that only integers and single-character strings can be set as a graphics parameter (and not NA nor NULL).}
  
  \item{pcex}{A real number giving the character expansion for the symbol: see \code{\link{par}}; default: 1.}
  
  \item{tipcex}{A real number giving the character expansion for the tip labels: see \code{\link{par}}; default: 1.}
  
  \item{plottype}{A character string: default behaviour is achieved with any string \emph{except} for (1) \code{plot == invisible}: nothing is plotted, only the ploting device is prepared; (2) \code{plot == invisible}: the plot is issued \emph{upon} existing plots (e.g. HPD bars) in the graphical device (see \code{\link{plot.phylo.upon}}).}
  
  \item{...}{Graphical parameters such as \code{lwd}: see \code{\link{par}}.}

}

\value{
None; the results are plotted on the current graphical device
}

\author{Christoph Heibl}

\seealso{
\code{\link{all.equal.phylo}}: S3 method for global comparison of class \code{phylo} objects.
}

\examples{
# 2 phylogenetic trees of Vipera
# ------------------------------
data(vipera)

# Have a look at both trees individually
# --------------------------------------
plot.phylo(vipera.bayes)
plot.phylo(vipera.ml)

# ... and compare them:
# --------------------
compare.phylo(vipera.bayes, vipera.ml, pcex = 1.5, lwd = 2)

# create modified Bayesian tree
# -----------------------------
vipera.modified <- shiftNodeAge(vipera.bayes, 14, 0.02)
vipera.modified <- shiftNodeAge(vipera.modified, 13, -0.03)
vipera.modified$tip.label[3:4] <- vipera.modified$tip.label[4:3]

# ... and again compare them:
# ---------------------------
compare.phylo(vipera.bayes, vipera.modified, pcex = 1.5, lwd = 2)

# use of the 'plot' argument:
# ---------------------------
compare.phylo(vipera.bayes, vipera.modified, pcex = 1.5, lwd = 2,
    plot = "invisible")
# HPDbars(vipera.bayes) # tree has no HPD summary
box.clades(vipera.bayes, nodes = 15, border = "grey60")
compare.phylo(vipera.bayes, vipera.modified, pcex = 1.5, lwd = 2,
    plot = "upon")
}
