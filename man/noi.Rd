\name{noi}
\Rdversion{1.1}
\alias{noi}

\title{Identification of MRCAs for Taxon Sets}

\description{
This function identifies the most recent common ancestor (MRCA) nodes for one or more sets of taxa/tips.
}

\usage{
noi(phy, group, regex = FALSE)
}

\arguments{
  \item{phy}{an object of class \code{\link[ape]{phylo}}.}
  
  \item{group}{a vector or list of vectors of mode \code{character} specifying the taxon set(s).}
  
  \item{regex}{a logical, if \code{regex = TRUE}, taxon sets are matched to the tip labels as regular expressions of the form \code{"taxon1|taxon2"}; otherwise strings will be matched exactly (see \code{\link{which}}).}
  
}

\value{
A vector of mode \code{"numeric"}.	
}

\author{Christoph Heibl}

\seealso{\code{\link[ape]{mrca}};
\code{\link{edge.color}} and \code{\link{tip.color}} to create color vectors for tree plotting, \code{\link{descendants}} for the contrary operation to \code{noi}.
}

\examples{
# molecular phylogeny of Eurasian vipers:
# ---------------------------------------
data(viperidae)	

# group can be either of mode 'numeric' or 'character'
# ----------------------------------------------------
numtax <- c(19, 31)
chartax <- c("Vipera_berus", "Vipera_wagneri")
node1 <- noi(viperidae, numtax)
node2 <- noi(viperidae, chartax)

# show results
# --------------------
tcol <- tip.color(viperidae, chartax, col = "blue")
tcol[numtax] <- "red"
plot(viperidae, tip.color = tcol)
nodelabels("MRCA1", node = node1, col = "red", cex = 0.8)
nodelabels("MRCA2", node = node2, col = "blue", cex = 0.8)

# the 'group' argument can also take regular expressions
# ------------------------------------------------------
rex <- "aspis"
node <- noi(viperidae, rex, regex = TRUE)
plot.phylo(viperidae, tip.color = 0, edge.color = 0)
box.clades(viperidae, nodes = node, col = "#D2A6A7", align = "all")
plot.phylo.upon(viperidae)
nodelabels(node = node, pch = 21, cex = 1.2, col = "red", bg = "#D2A6A7")

# if the 'group' argument is a list of elements of length 2,
# n = length(group) nodes of interest will be returned
# ----------------------------------------------------
group <- list(
    c("Vipera_berus", "Vipera_ursinii"),
    c("Vipera_aspis_ssp._aspis", "Vipera_latastei"),
    c("Vipera_ammodytes_ssp._ammodytes", 
        "Vipera_ammodytes_ssp._montandoni"),
    c("Macrovipera_lebetina", "Vipera_wagneri")
)
clades <- noi(viperidae, group)
plot.phylo(viperidae, tip.color = 0, edge.color = 0)
box.clades(viperidae, nodes = clades, col = c("#FFFFA5", "#D2A6A7",
    "#A7D2A5", "#A5A6D2"), align = "all")
plot.phylo.upon(viperidae)
}
