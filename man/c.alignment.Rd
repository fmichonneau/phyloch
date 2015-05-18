\name{c.sequences}
\alias{c.alignment}
\alias{c.genes}

\title{Concatenation of DNA Sequences}

\description{
\code{c.alignment} concatenates two or more sets of sequences of the \bold{same gene partition} into a single object of class \code{alignment}. 

\code{c.genes} concatenates two or more sets of sequences representing \bold{different gene partitions}.
}

\usage{
\method{c}{alignment}(...)

\method{c}{genes}(..., single.list, match = TRUE)
}

\arguments{
  \item{...}{two or more object of class \code{alignment} or \code{DNAbin} separated by commas.}
  
  \item{single.list}{list object whose elements are of class \code{DNAbin}}
  
  \item{match}{logical, if \code{match = TRUE} only those sequences present in all partitions will be concatenated; if \code{match = FALSE} the concatenated alignment will contain all sequences.}
  
}

\value{
  An object of classes \code{alignment} or code{DNAbin}.
}

\author{Christoph Heibl}

\note{Sequences are matched by their named, therefore sequence names require identical spelling if they are to be concatenated.
	}

\seealso{
	\code{\link{read.fas}} and \code{\link{write.fas}} for reading and writing DNA sequence files.
}

\examples{
# examples for c.genes
# --------------------

data(woodmouse)
w1 <- woodmouse[, 1:250]
w2 <- woodmouse[, 251:750]
w3 <- woodmouse[, 751:965]

w <- c.genes(w1, w2, w3)

w.list <- list(w1, w2, w3)
W <- c.genes(single.list = w.list)

identical(w, W)
}

