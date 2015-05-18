\name{homogen.base.freq}
\Rdversion{1.1}
\alias{homogen.base.freq}

\title{Homogeneity of base frequencies}
\description{
This function takes a set of aligned DNA sequences and performs a Chi-square test in order to assess the homogeneity of base frequences among taxa.}

\usage{
homogen.base.freq(x, tabulate.only = FALSE)
}


\arguments{
  \item{x}{object of class \code{DNAbin}}
  \item{tabulate.only}{logical}
}

\details{
Sites with missing or ambiguous data are discarded from the alignment prior to the tabulation of bases.
}

\value{
	see \code{chisq.test}
}

\author{Christoph Heibl}


\seealso{
\code{\link{delete.empty.cells}}, \code{\link{delete.gaps}}
}

