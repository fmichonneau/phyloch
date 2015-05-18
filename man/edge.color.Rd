\name{tree.coloring}
\Rdversion{1.1}
\alias{tip.color}
\alias{edge.color}

\title{Colors for Tips and Edges in Phylogenies}

\description{
This functions create vectors with color names that can be used as input for the \code{tip.color} and \code{edge.color} arguments of \code{\link[ape]{plot.phylo}} from the \code{ape} package.
}

\usage{
tip.color(phy, tips, regex = FALSE, col, bgcol)

edge.color(phy, groups, what = "crown", col, bgcol)
}

\arguments{
  \item{phy}{an object of class \code{\link[ape]{phylo}}.}
  
  \item{groups}{vector, matrix, or list, giving the groups, either monophyletic or not, whose edges will be colored. The elements of \code{groups} may be of mode \code{numeric} or \code{character}, representing tip numbers or tip labels.
}

  \item{tips}{a vector or list of mode \code{character}, giving the tip labels to be colored.}
  
  \item{regex}{logical: if true, all elements of \code{tips} are interpreted as regular expressions. \code{regex = FALSE} corresponds to \code{fixed = TRUE} in \code{\link{grep}}.}

  \item{bgcol}{a character string, giving the default color, i.e. the color for those edges and tips not present in \code{groups} or \code{tips}.}
  
  \item{col}{a vector of mode \code{character}, containing the colors to be used, will be recycled.}
  
  \item{what}{a character string, indicating whether the stem lineage of a clade shall be included (\code{what = "stem"}) or not (\code{what = "crown"}).}
}

\value{
a vector of mode \code{character}.
}

\author{Christoph Heibl}

\seealso{\code{\link{plot.phylo}} for plotting of phylogenies; \code{\link{clade.bars}}, \code{\link{box.clades}}, and \code{\link{box.tips}} for high-lighting clades in phylogenies; \code{\link{taxo.fonts}} for setting nomenclaturally proper tip labels. 
}

\examples{
# phylogentic relationships of bird orders:
data(bird.orders)

## 1. EXAMPLE: three monophyletic clades
## -------------------------------------

# define clades:
clades <- list(1:5, 8:12, 13:23)
	
# generate edge colors:
mycols <- c("orange", "purple", "blue")
ecol <- edge.color(bird.orders, clades, col = mycols)

# plot tree:
plot(bird.orders, edge.color = ecol,
  no.margin = TRUE, edge.width = 2)

## 2. EXAMPLE: depict patristic distance between two tips
## ------------------------------------------------------

group <- c("Strigiformes", "Piciformes")
ecol <- edge.color(bird.orders, group, col = "red")
tcol <- tip.color(bird.orders, group, col = "red")
plot(bird.orders, edge.color = ecol, tip.color = tcol,
  no.margin = TRUE, edge.width = 2)

}
