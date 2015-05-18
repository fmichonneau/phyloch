\name{read.fas}
\alias{read.fas}
\alias{read.nex}
\alias{read.phy}

\title{Read DNA Sequences}

\description{
This functions parse DNA sequences in FASTA, PHILIP, and NEXUS formatted files.
}

\usage{
read.fas(x, text)

read.nex(x)

read.phy(x)
}

\arguments{
  \item{x}{a character string, giving the file name.}
  \item{text}{a character string in FASTA format.}
}

\value{
  An object of class \code{DNAbin} and of mode \code{matrix} if all sequences are of same length or \code{matrix} if they are not.
}

\references{
Maddison, D.R., D.L. Swofford, and W.P. Maddison. 1997. NEXUS: an extensible file format for systematic information. \emph{Syst. Biol.} \bold{46}: 590-621.

}

\author{Christoph Heibl}

\seealso{\code{\link{mafft}} and \code{\link{prank}} for sequence alignment, \code{\link{gblocks}} and \code{\link{aliscore}} for quality check and cleaning of sequence alignments, \code{\link{c.genes}} for concatenation of sequence alignments.}

\examples{
# sequence data:
data(woodmouse)

# write sequences files
write.fas(woodmouse, "phyloch.fas")
write.phy(woodmouse, "phyloch.phy")
write.nex(woodmouse, "phyloch.nex")

# read sequence files
fas <- read.fas("phyloch.fas")
phy <- read.phy("phyloch.phy")
nex <- read.nex("phyloch.nex")

# remove sequence files
unlink("phyloch.*")
}
