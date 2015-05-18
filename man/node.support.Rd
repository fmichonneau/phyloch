\name{node.support}
\alias{node.support}

\title{Visualizing Node Support on Phylogenetic Trees}

\description{
This function plots support values on phylogenetic trees drawn with \code{\link{plot.phylo}}. Support values above a given cut-off value can be depicted as numbers, colored dots, or thickened branches.}

\usage{
node.support(x, transfer, cutoff = 0, digits, mode = "numbers", font = 2, 
	pos = "pretty", cex = 0.8, col, legend = FALSE, node, ...)
}

\arguments{
	
  \item{x}{A vector of mode \code{"numeric"} containing node support values.}
  
  \item{transfer}{A list containing the two objects of class \code{"phylo"}; the first be the \bold{source} tree, where \code{x} is taken from, the second being the \bold{target} tree on which \code{x} will be plotted.}
  
  \item{cutoff}{A numeric, only support values >= \code{cutoff} will be considered for plotting.}
  
  \item{digits}{an integer giving the number of digits (see \code{\link{round}})}
  
  \item{mode}{Either \code{"numbers"}, \code{"dots"}, or \code{"edges"} indicating the way how node support will be plotted (see Examples).}
  
  \item{font}{Fonts to be used for if \code{mode = "numbers"}: 1 corresponds to plain text (the default), 2 to bold face, 3 to italic, and 4 to bold italic.}
  
  \item{pos}{Position of text if \code{mode = "numbers"}: \code{"above"} or \code{"below"} the subtending edge or to the "right" of the node; if \code{pos = "pretty"}, text is positioned in a way that tries to minimize touching or intersecting edges.
  	}
  
  \item{cex}{A numeric giving font size, dot size, or thickness of braches depending on the choice of the \code{mode} argument.}
  
  \item{col}{A character string, giving one or more colors, depending on the choice of the \code{mode} argument.}
  
  \item{legend}{Logical, if \code{TRUE} and \code{mode = "numbers"} a legend is added to the plot.}
  
  \item{node}{A vector of integers giving the number of nodes for which support will displayed; default is all interior nodes.}
  
  \item{\dots}{Further graphical parameters (from \code{\link{par}}).}
}

\author{Natalie Cusimano, modified by Christoph Heibl}

\note{
	\code{node.support} requires a previous call of \code{\link{plot.phylo}} in order to work.
	
	The \code{transfer} argument has worked well with example cases I could think of, but has not been tested thoroughly with real data, so be careful and double-check the resulting output. 
}

\seealso{\code{\link{plot.phylo}}, \code{\link{raxml}}}

\examples{
# phylogenetic tree of wild barley
# --------------------------------
data(hordeum)
hordeum$tip.label <- gsub("H[a-z]+", "H._", hordeum$tip.label)

# plot tree and posterior node support 
# settingchange color, cut-off, and position:
# -------------------------------------------
plot(hordeum)
node.support(hordeum$posterior, cutoff = 0.9, pos = "right",
	col = "red")
	
# you can display up to three support values:
# -------------------------------------------
plot(hordeum)
node.support(hordeum $posterior, cutoff = 0.9, pos = "right",
	col = "red", font = 2)
node.support(hordeum $posterior, cutoff = 0.9, pos = "above",
	col = "blue", font = 1)
node.support(hordeum $posterior, cutoff = 0.9, pos = "below",
	col = "green", font = 3)
	
# the pretty option of the position argument:
# -------------------------------------------
plot(hordeum)
node.support(hordeum $posterior, pos = "pretty")
	
# display support as colored nodes ...
# ...by omitting nodes below threshold
# ------------------------------------
plot(hordeum)
node.support(hordeum $posterior, cutoff = 0.9, mode = "dots",
    col = "black")
# ... or with two colors:
# -----------------------
plot(hordeum)
node.support(hordeum $posterior, cutoff = 0.9, mode = "dots",
    col = c("black", "red"))
    
# display support as bold edges:
# ------------------------------
plot(hordeum)
node.support(hordeum $posterior, cutoff = 0.9, mode = "edges",
    col = "black", cex = 5)	
		
# display support values from a different source tree
# on a target tree:
# -----------------
data(vipera)
# plot maximum likelyhood tree and add 
# bootstrap values from the same tree:
# ------------------------------------
plot(vipera.ml)
node.support(vipera.ml$node.label, pos = "above")
# now transfer posterior probabilities 
# from a Bayesian tree:
# ---------------------
node.support(vipera.bayes$node.label, 
    transfer = list(vipera.bayes, vipera.ml), pos = "below")
    
# this would have been the result without 
# using the transfer argument:
# ----------------------------
node.support(vipera.bayes$node.label, col = "red", pos = "right")	
}