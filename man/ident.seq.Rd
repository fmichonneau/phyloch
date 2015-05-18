\name{ident.seq}
\alias{ident.seq}
\title{Identify or delete identical DNA sequences}
\description{
This functions identifies identical sequences in an alignment of DNA sequences. In each subset of identical sequences all but the first identical sequences are deleted from the alignment.}
\usage{
ident.seq(x, quiet = TRUE)
}
\arguments{
  \item{x}{An object of class \code{DNAbin}.}
  \item{quiet}{Logical, if \code{TRUE} number and name the deleted sequences are printed out on the console}
}
\value{
An object of class \code{DNAbin}.}

\author{Christoph Heibl}

\seealso{\code{\link{delete.empty.cells}} to identify and delete empty rows and columns, i.e., rows and columns containing only Ns.}

\examples{
data(woodmouse)
seq <- woodmouse[c(1, 2, 3, 3 , 3, 4, 4), ]
seq2 <- ident.seq(seq, quiet = FALSE)
rownames(seq2)
}