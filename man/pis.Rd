\name{pis}
\alias{pis}
\title{Number of parsimony-informative sites}

\description{
This function returns the number of potentially-informative(parsimony-informative) sites in DNA sequence alignment}

\usage{
pis(x, abs = TRUE, use.ambiguities = FALSE)
}

\arguments{
	
  \item{x}{object of class \code{DNAbin}}
  
  \item{abs}{logical, should the absolute or the relative number number be returned}
  
  \item{use.ambiguities}{logical, currently only \code{FALSE} is implemented, which means that IUPAC ambiguity symbols will be treated as missing information (N)}
}

\value{
  numeric (the number or percentage of potentially-informative nucleotide sites)
}

\author{Christoph Heibl}

\seealso{\code{\link{homogen.base.freq}}}

\examples{
# example data:
# -------------
data(woodmouse)	

# number of potentially-informative sites:
# ----------------------------------------
pis(woodmouse, abs = TRUE)
	
# proportion of potentially-informative sites:
# --------------------------------------------
pis(woodmouse, abs = FALSE)
}
