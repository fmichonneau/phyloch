\name{raxml}
\alias{raxml}

\title{Maximium Likelihood Tree Search}

\description{
This function calls the RAxML algorithm (A. Stamatakis, 2006 onwards) to find the maximum likelihood estimate of tree topology and branch lengths for a given DNA sequence alignment. It can also do reasonably rapid non-parametric bootstrapping.
}

\usage{
raxml(x, runs = 10, partition = NULL, outgroup = NULL, 
      backbone = NULL, optimize = FALSE, clear = TRUE, 
      file = "fromR", path = "/Applications/RAxML-7.0.4")
}

\arguments{
  \item{x}{A matrix of DNA sequences of class \code{DNAbin}.}
  
  \item{runs}{A vector of mode \code{"numeric"} (see details).}
  
  \item{partition}{A \code{data frame} specifying partitions of the alignment.}
  
  \item{outgroup}{A vector of mode \code{"character"} containing the names of the outgroup taxa. Must be given when bootstrapping!}
  
  \item{backbone}{A \code{phylo} object handed as a backbone tree to RAxML.}
  
   \item{optimize}{A logical; if \code{TRUE} parameteres are optimized before starting analysis. VERY time-consuming!}
   
    \item{clear}{A logical indicating if the RAxML output files should be cleared from the RAXML directory and transferred to another directory created automatically in the current R working directory.}
    
  \item{file}{A vector of mode \code{"character"} giving a name to the output files.}
  
  \item{path}{A vector of mode \code{"character"} giving the absolute path to the directory containing the RAXML executable.}
  
}

\details{
\code{runs} is a numeric vector either of length 1 or 2. The first number defaults to 10 and gives the number of tree searches that are conducted in order to find the best maximum likelihood estimate of the phylogenie. The second number - if present - gives the number of non-parameteric bootstrap replicates. The bipartitions found in these bootstraps are subsequently mapped on the best topology found in the previous step.

If you are doing non-parametric bootstraps and you decide to stop the analysis before finishing the specified number of runs, you can use the UNIX command \code{kill} to do so. Bipartitions are then calculated from the existing replicates and the output is stored in exactly the same manner as as in a nonterminated analysis. 
}

\value{
  an object of class \code{phylo}
}
% REFERENCES
\references{
Stamatakis, A. 2006. RAxML-VI-HPC: 
Maximum likelihood-based phylogenetic analyses with thousands of taxa and mixed models. \emph{Bioinformatics} \bold{22}: 2688-2690.}
% AUTHOR
\author{Christoph Heibl}
% NOTE
\note{
RAxML is a C program and the source code is not contained in this package. This means that in order to run this function you will need to install RAxML yourself. See the 'Software' tab on \url{http://www.exelixis-lab.org/} for the most recent documentation and source code of RAxML. Depending on where you chose to install RAxML, you need to adjust the \code{path} argument.

\code{raxml} was last tested and running fine with the alpha release of RAxML 7.2.8. 
}
% SEEALSO
\seealso{
\code{\link{node.support}} for visualization of non-parametric bootstrap values on a phylogenetic tree.
}
% EXAMPLES
\examples{
## not run
# data(woodmouse)
# MLtree <- raxml(woodmouse, runs = c(2, 5))
# plot.phylo(MLtree)
}