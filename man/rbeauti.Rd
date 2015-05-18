\name{rbeauti}
\alias{rbeauti}

\title{XML Input Files for BEAST}

\description{
BEAST uses an MCMC approach to estimate rooted phylogenies from molecular data (Drummond & Rambaut, 2007). This function is intended to prepare XML files for BEAST with R. See \code{note} section for currently supported models.}

\usage{
rbeauti(path = "aaa.xml", s, taxonset = NULL, monophyly = NULL,
  tmrcaCons = NULL, startingTree = "random", specModel = "yule",
  clock = "strict", ngen = "10000000", samplefreq = "10000",
  logSubs = FALSE, nodata = FALSE)
}

\arguments{
	
  \item{path}{A character string, defines directory and filename for the XML input file.}
  
  \item{s}{An object of class \code{DNAbin}.}
  
  \item{taxonset}{A list giving the taxon sets.}
  
  \item{monophyly}{A vector indicating monophyly constraints for the taxon sets declared with \code{taxonset}.}
  
  \item{tmrcaCons}{A list, containing the prior distribution(s) for age constraints of internal nodes (which must be grouped by \code{taxonset}).}
  
  \item{startingTree}{Either "random" or "upgma", or an object of class \code{"phylo"} to be used as a starting tree.}
  
  \item{specModel}{A character string indicating an evolutionary model to construct a prior distribution of node heights (tree prior). Currently implemented are the \bold{Yule Model} (\code{"yule"}), the \bold{Birth-Death Model} (\code{"birthDeath"}), and the \bold{Coalescent model with constant size} (choosen with any other string).}
  
  \item{clock}{A character string, either \code{"strict"} or \code{"lognormal"} to choose between the strict clock and the uncorrelated lognormal relaxed clock model. The exponential rates relaxed clock is currently not supported.}
  
  \item{ngen}{A character string, the number of generations to run the MCMC.}
  
  \item{samplefreq}{A character string, the intervals between sampling the MCMC.}
  
  \item{logSubs}{A logical, indicating if trees with branch lengths expressed as substitution should be logged.}
  
  \item{nodata}{A logical, indicating if BEAST should be run without data (see details).} 
}

\details{
  <add details ... >
  
  Bayesian analyses should be run with and without data, to test the effect of the prior on the results. A XML file that runs without data can be obtained by setting \code{nodata = TRUE}. See: \url{http://beast.bio.ed.ac.uk/Sampling_the_Prior}. 
}


\references{
The BEAST v.1.5.x XML reference: \url{http://beast.bio.ed.ac.uk/BEAST_v1.5.x_XML_Reference}	
	
Drummond, A.J. & A. Rambaut. 2007. BEAST: Bayesian evolutionary analysis by sampling trees. \emph{BMC Evolutionary Biology} \bold{7}: 240.
}

\author{Christoph Heibl}

\note{
	
	\code{rbeauti} has been tested with \bold{BEAST 1.5.3}.
	
	Currently, there are certain limitations of \code{rbeauti} compared to BEAUTi:
	
	Only the GTR model is supported.
	
	The exponential relaxed clock model is not supported.
	
	Only the tree priors 'Speciation: Yule', 'Speciation: Birth-Death', and 'Coalescent: Constant' are implemented.
	
	On the other hand, \code{rbeauti} accepts a user-defined starting tree. In order to avoid imcompatibilities between this starting tree and other priors, this tree should (but sometimes does not need to) be ultrametric satisfying all the priors on node heights that will be used in BEAST. 
	
	\code{rbeauti} also incorporates the rules of thumb for setting operator weights given in 'A Rough Guide to BEAST 1.4' (p. 15).
	
}

\seealso{\code{\link{read.beast}}}