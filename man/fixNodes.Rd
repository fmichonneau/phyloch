\name{fixNodes}
\Rdversion{1.1}
\alias{fixNodes}

\title{
Standard Node Numbering in Phylo Objects
}
\description{
The function (re-)establishes the standard numbering of terminal and internal nodes in phylogenies represented as objects of class \code{\link[ape]{phylo}}.
}

\usage{
fixNodes(phy, quiet = TRUE)
}
 
\arguments{
  \item{phy}{an object of class \code{\link[ape]{phylo}}. 
	}
  \item{quiet}{logical: if \code{TRUE} (default), screen output is suppressed. Intended for possible debugging, may be removed later.}
}

\details{
When reading in phylogenetic trees from a NEXUS file that contains a \code{translate} section, it can happen that the terminal nodes (tips, leaves) of the corresponding \code{phylo} object are not numbered consecutively, which can be a problem in some downstream applications. You can use \code{fixNodes} to get the correct order of terminal node numbers.

\code{fixNodes} is also intended to re-establish the standard numbering of internal nodes and reorder all node value elements (e.g. node.label, posterior, ...) if a \code{\link[ape]{phylo}} object has been modified by either \code{\link[ape]{root}}, \code{\link[ape]{ladderize}}, or \code{\link[ape]{rotate}}.
}

\value{an object of class \code{\link[ape]{phylo}}.}

\author{Christoph Heibl}

\seealso{\code{\link[ape]{read.tree}}, 
	\code{\link{read.nexus}}, \code{\link{read.beast}} for reading trees; \code{\link[ape]{ladderize}} and \code{\link[ape]{rotate}} for tree manipulation; \code{\link{node.support}} for plotting node support values; \code{\link{node.trans}} for handling non-standard node elements in class \code{\link[ape]{phylo}}.
}

\examples{
# phylogenetic tree of wild barley
# -------------------------------- 
data(hordeum)
hordeum$tip.label <- LETTERS[1:20]
hordeum $edge.length <- NULL

# split plotting device
# ---------------------
pp <- par(c("mfcol", "mar"))
par(mfcol = c(1, 2), mar = c(0.5, 0.5, 0.5, 0.5))

## EXAMPLE 1:

# rerooting of tree changes numbering of 
# terminal nodes
# --------------------------------------
rerooted.tr <- root(hordeum, outgroup = c("L"), 
	resolve.root = TRUE)	
plot(rerooted.tr, label.offset = strwidth("--"))
nodelabels(cex = 0.7)
tiplabels(cex = 0.7)

# reset to standard numbering
# ---------------------------
#fixed.tr <- fixNodes(rerooted.tr)	
#plot(fixed.tr, label.offset = strwidth("---"), direction = "left")
#nodelabels(cex = 0.7)
#tiplabels(cex = 0.7)

## EXAMPLE 2:

# ladderization of tree changes numbering of 
# internal and terminal nodes
# ------------------------------------------
ladderized.tr <- ladderize(hordeum)	
plot(ladderized.tr, label.offset = strwidth("--"))
nodelabels(cex = 0.7)
tiplabels(cex = 0.7)

# reset to standard numbering
# ---------------------------
fixed.tr <- fixNodes(ladderized.tr)
plot(fixed.tr, label.offset = strwidth("---"), direction = "left")
nodelabels(cex = 0.7)
tiplabels(cex = 0.7)

# reset graphical parameters
# --------------------------
par(pp)

}
