\name{node.trans}
\Rdversion{1.1}
\alias{node.trans}

\title{Translate Internal Node Indices between Trees}

\description{
This function compares and translates internal node indices for two objects of class \code{\link[ape]{phylo}}. 
}

\usage{
node.trans(source, target, index = FALSE)
}

\arguments{
	
  \item{source}{
An object of class \code{\link[ape]{phylo}}.
}
  \item{target}{
An object of class \code{\link[ape]{phylo}}.
}
  \item{index}{Logical (see description).}
}

\details{
\code{node.trans} requires identical sets of tips in both trees. It can handle unresolved trees, but will sometimes fail if the target tree has more internal nodes, i.e., is better resolved, than the source tree.  

If used with \code{index = TRUE}, \code{node.trans} behaves like the \code{\link{match}} function, returning a vector of indices with can be used to order node labels of the source tree to fit with the node order of the target tree. Alternatively, \code{index = FALSE} returns a matrix with corresponding node indices in both trees.
}

\author{Christoph Heibl}

\seealso{
\code{\link{match}}, \code{\link{fixNodes}}, \code{\link{node.support}}.
}

\examples{	
# phylogenetic trees of Vipera
# ----------------------------
data(vipera)

# set graphic parameters
# ----------------------
opar <- par(no.readonly = TRUE)
par(mar = c(0, 0, 0, 0), mfrow = c(2,1))

# show 'ursinii-clade' and posterior
# probabilites on Bayesian tree
# ----------------------------------
ursiniiClade <- list(c("Vipera_kaznakovi", "Vipera_ursinii", "Vipera_dinniki"))
tcol <- tip.color(vipera.bayes, ursiniiClade, col = "red")
plot(vipera.bayes, tip.color = tcol)
node.support(vipera.bayes$node.label, col = "blue")

# show 'ursinii-clade' and bootstrap
# values on ML tree
# ----------------------------------
tcol <- tip.color(vipera.ml, ursiniiClade, col = "red")
plot(vipera.ml, tip.color = tcol)
node.support(vipera.ml$node.label, pos = "above")

# show posterior probabilites on ML tree
# with help of 'node.trans'
# --------------------------------------
id <- node.trans(vipera.bayes, vipera.ml, index = TRUE)
node.support(vipera.bayes$node.label[id], pos = "below", col = "blue")

# there is a short cut in node.support
# ------------------------------------
plot(vipera.ml, tip.color = tcol)
node.support(vipera.ml$node.label, pos = "above")
node.support(vipera.bayes$node.label, pos = "below",
    transfer = list(vipera.bayes, vipera.ml))
    
# reset graphic parameters
# ------------------------
par(opar)
}
