\name{fillEndsWithN}
\alias{fillEndsWithN}
\title{Fill alignment ends with N}

\description{
This function can be used to fill both ends of a DNA aligment of class \code{DNAbin} with a character specified in \code{sym}.
}

\usage{
fillEndsWithN(x, sym = "n")
}

\arguments{
  \item{x}{object of class \code{DNAbin}}
  \item{sym}{character string giving the symbol with which to fill    the missing nucleotides}
}

\value{
  object of class \code{DNAbin}
}

\author{Christoph Heibl}

\seealso{\code{\link{delete.empty.cells}}}

\examples{
# simple example alignment:
# -------------------------
x <- structure(list(nb = 3, 
	seq = c("aaaaaaaaaa", "--aa--aa--", "tttttttttt"), 
	nam = c("A", "B", "C")), .Names = c("nb", "seq", "nam"), 
	class = "alignment")

# convert to DNAbin:
# ------------------
x <- as.DNAbin(x)

# fill missing nucleotides:
# -------------------------
x <- fillEndsWithN(x)

# show results:
# -------------
as.character(x[2, ])
}
