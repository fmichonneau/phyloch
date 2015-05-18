\name{delete.gaps}
\Rdversion{1.1}
\alias{delete.gaps}

\title{Remove Indel Positions from an Alignment}

\description{
This function removes indel positions (gaps) from a sequence alignment.
}

\usage{
delete.gaps(x)
}

\arguments{
  \item{x}{an object of class \code{\link{DNAbin}}.}
}

\details{
\code{delete.gaps} removes every column (position) that contains the gap character (\code{"-"}) for at least one sequence in the alignment. The ouput is always an alignment of sequences of equal length. In contrast, \code{\link[ape]{del.gaps}} removes all gap characters from the alignment, so the result will be (most probably) never a set of sequences of equal length.
}

\value{
an object of class \code{\link{DNAbin}}
}

\author{Christoph Heibl}

\seealso{
\code{\link{code.simple.gaps}} for coding of simple gaps, \code{\link[ape]{del.gaps}} for removal of all gap symbols from an alignment, \code{\link{gblocks}} and \code{\link{aliscore}} for more sophisticated  methods of cleaning alignments.
}