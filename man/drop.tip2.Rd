\name{drop.tip2}
\alias{drop.tip2}

\title{Remove Tips from a Phylogenetic Tree}
\description{
As \code{drop.tip} from the ape package, \code{drop.tip2} removes terminal branches of a phylogenetic tree, possibly removing the corresponding internal branches, but, in addition, can handle further, non-standard list elements of \code{"phylo"} object (i.e. vectors of length \code{phy$Nnode}, which may contain support values, ages or rate estimates, etc. ).
}

\usage{
drop.tip2(phy, tip, trim.internal = TRUE, subtree = FALSE, 
  root.edge = 0, rooted = is.rooted(phy))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phy}{
An  object of class \code{"phylo"}.
}
  \item{tip}{
A vector of mode numeric or character specifying the tips to delete.
}
  \item{trim.internal}{
A logical specifying whether to delete the corresponding internal branches.
}
  \item{subtree}{
A logical specifying whether to output in the tree how many tips have been deleted and where.
}
  \item{root.edge}{
An integer giving the number of internal branches to be used to build the new root edge. This has no effect if \code{trim.internal = FALSE}.

}
  \item{rooted}{
A logical indicated whether the tree must be treated as rooted or not. This allows to force the tree to be considered as unrooted (see examples in \code{\link{drop.tip}}).
}
}
\details{
See \code{\link{drop.tip}} for details.}

\value{
An object of class \code{"phylo"}.
}

\author{
Christoph Heibl based on \code{drop.tip} by Emanuel Paradis.
}

\seealso{
\code{\link{extract.clade2}}, \code{\link{drop.tip}}
}

\examples{
# Hordeum species tree
# --------------------
data(hordeum)

# These tips ... 
# --------------
tips2remove = c("Hordeum_pubiflorum", "Hordeum_patagonicum",
  "Hordeum_comosum", "Hordeum_gussoneanum","Hordeum_marinum")

# ... will be pruned from the tree:
# ---------------------------------
hordeum_pruned <- drop.tip2(hordeum, tips2remove)

# Compare the results using the height parameter 
# from a divergence time analysis:
# ----------------------------------------------
org.par <- par()
par(mfrow = c(2,1), mar = c(0, 0, 1, 0))

tcol <- tip.color(hordeum, list(tips2remove), 
	col = "red")
plot.phylo(hordeum, tip.color = tcol)
title("Hordeum species tree")
nodelabels(hordeum$height, cex = 0.8)

plot.phylo(hordeum_pruned)
title("Hordeum species tree with tips removed")
nodelabels(hordeum_pruned$height, cex = 0.8)
par(org.par)
}