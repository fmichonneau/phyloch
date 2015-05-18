\name{geophylo}
\Rdversion{1.1}
\alias{geophylo}

\title{3D Geophylogenies for GIS Applications}

\description{
Given an ultametric phylogeny and locality data for each of its tips, this function constructs a three-dimensional vector map that can be displayed in GIS applications on raster surfaces or topological maps (e.g. via the NVIZ model in GRASS GIS).
}

\usage{
geophylo(phy, file = "", loc, z_exag = 1000)
}

\arguments{
  \item{phy}{An object of class \code{phylo}, must be ultrametric.}
  
  \item{file}{A character string giving the filename; if \code{file = ""}, the file is written on the standard output connection (i.e. the console).}
  
  \item{loc}{A matrix with locality data for the tips of the phylogeny; rows are assumed to correspond to tips as numbered in objects of class \code{phylo}; the first column must contain longitudinal values, the second column latitudinal values.}
  
  \item{z_exag}{Integer; used for superelevation of the internal nodes.
}
}

\value{
none; a vector map in ASCII format in printed to the given directory
}

\references{This function is inspired by \code{GeoPhyloBuilder}, a plug-in developed for ArcGIS by David Kidd and Xianhua Liu, available at \url{https://www.nescent.org/wg_EvoViz/GeoPhyloBuilder.}
	
Kidd, D. & X. Lui. 2008. GEOPHYLOBUILDER 1.0: an ArcGIS extension for creating 'geophylogenies'. \emph{Molecular Ecology Resources} \bold{8}:88-91.
}

\author{Christoph Heibl}

\seealso{
\code{\link{read.tree}}
}
