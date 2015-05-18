\name{mrbayes}
\Rdversion{1.1}
\alias{mrbayes}
\alias{mrbayes.mixed}

\title{Bayesian MCMC Tree Search with MrBayes}

\description{
This function provides a wrapper to Bayesian phylogenetic tree search through MrBayes (Ronquist & Huelsenbeck, 2003) with either DNA (\code{mrbayes}) or morphological (\code{mrbayes.mixed}) data.
}

\usage{
mrbayes(x, file = "", nst = 6, rates = "invgamma", ngammacat = 4, 
    nruns = 2, ngen = 1e+06, printfreq = 100, samplefreq = 10, 
    nchains = 4, savebrlens = "yes", temp = 0.2, burnin = 10, 
    contype = "allcompat", run = FALSE)


mrbayes.mixed(x, file, nst = 6, rates = "invgamma", ngammacat = 4, nruns = 2, 
	ngen = 1e+06, printfreq = 100, samplefreq = 10, nchains = 4,
	savebrlens = "yes", temp = 0.2, burnin = 10, contype = "allcompat", 
	path = "/Applications/mrbayes-3.1.2/", run = TRUE)
}

\arguments{
  \item{x}{object of class \code{DNAbin} in the case of \code{mrbayes} or a matrix of mode character in the case of \code{mrbayes.mixed}}
  
  \item{file}{character string, giving the name of the MrBayes input file}
  
  \item{nst}{integer giving the number of rates in the model of sequence evolution}
  
  \item{rates}{character string; allowed are \code{"equal"}, \code{"gamma"}, \code{"propinv"}, \code{"invgamma"}, and \code{"adgamma"}; the default is \code{"equal"}}
  
  \item{ngammacat}{integer; the number rate categories for the discretized Gamma distribution; the default is \code{4}}
  
  \item{nruns}{integer; the number of runs}
  
  \item{ngen}{integer; the number of states of the MCMC}
  
  \item{printfreq}{integer; the interval between states of the MCMC to be printed on the screen}
  
  \item{samplefreq}{integer; the interval between states of the MCMC to be \bold{sampled}
}

  \item{nchains}{integer; number of Metropolis coupled MCMCs in each run}
  
  \item{savebrlens}{logical; shall branch lengths be saved}
  
  \item{temp}{
%%     ~~Describe \code{temp} here~~
}

  \item{burnin}{integer; the number of samples from the MCMC to be discarded prior to further analysis}
  
  \item{contype}{character string; the type of consensus tree calculated from the posterior distribution of trees: either \code{"halfcompat"} (majority-rule consensus tree) or \code{"allcombat"} (strict consensus tree)
}

  \item{path}{characters string; the filename to be used for the MrBayes input file}
  
  \item{run}{logical; \code{run = FALSE} will only print the NEXUS file, \code{run = TRUE} will also start the MCMC runs, if the \code{path} argument is correctly specified}
}

\details{ 	
\code{mrbayes} was developed and tested with MrBayes \bold{v3.2.0} under R 2.14.1 on a Mac 10.6.8/32-bit platform. It is intended to offer a simply parameterized building block for larger scripts.	
}

\value{
	None; a NEXUS file with MrBayes block is written to a file and, if \code{run = TRUE}, the MCMC runs in MrBayes are started.
}

\references{
Ronquist F. & J. P. Huelsenbeck. 2003. MrBayes 3: Bayesian phylogenetic inference under mixed models. \emph{Biometrics} \bold{19}: 1572-1574.
	
MrBayes website: \url{http://mrbayes.scs.fsu.edu/}.
}

\author{Christoph Heibl}

\seealso{
\code{\link{mafft}} and \code{\link{prank}} for sequence alignment; \code{\link{raxml} for maximum likelihood tree search.}
}

\examples{	
# DNA sequence data:
# ------------------
data(woodmouse)
x <- woodmouse[,1:60] # tiny alignment

# print NEXUS file with MrBayes block to working directory
# --------------------------------------------------------
mrbayes(x, file = "", ngen = 100, run = FALSE)
    
}