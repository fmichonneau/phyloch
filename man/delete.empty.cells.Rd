\name{delete.empty.cells}
\alias{delete.empty.cells}

\title{Delete empty rows and columns from a DNA sequence alignment}

\description{
This function can be used to delete completely missing or ambiguous characters and taxa from a DNA sequence alignment.
}

\usage{
delete.empty.cells(alignment, nset = c("-", "n", "N", "?"), quiet = FALSE)
}

\arguments{
  \item{alignment}{an object of class \code{DNAbin}}
  
  \item{nset}{vector of mode character; rows or colums that consist \bold{only} of the characters given in \code{nset} will be deleted from the alignment}
  
  \item{quiet}{logical: if set to \code{TRUE}, screen output will be suppressed}
}

\value{
  object of class \code{DNAbin}
}

\author{Christoph Heibl}

\seealso{\code{\link{ident.seq}}}