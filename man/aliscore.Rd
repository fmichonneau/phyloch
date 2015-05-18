\name{aliscore}
\alias{aliscore}
\title{Cleaning of Sequence Alignments}

\description{
This function provides a wrapper to \bold{Aliscore}, which can be used remove problematic regions of a sequence alignment.
}

\usage{
aliscore(x, path)
}

\arguments{
  \item{x}{DNA sequences of class \code{DNAbin}.}

  \item{path}{A character string, giving the path to the Aliscore program.}
}

\value{
    \code{matrix} of class \code{"DNAbin"}
}

\references{
\url{http://zfmk.de/web/Forschung/Abteilungen/AG_Wgele/Software/Aliscore/Download/index.en.html}
}

\author{
Christoph Heibl
}

\note{
This function was developed with ALISCORE version 02. 
}

\seealso{
\code{\link{gblocks}} might also be used to clean alignment.
}
