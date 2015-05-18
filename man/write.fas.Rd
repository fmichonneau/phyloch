\name{write.fas}
\alias{write.fas}
\alias{write.phy}
\alias{write.nex}

\title{Write DNA Sequences to File}

\description{
These functions write DNA sequences to FASTA, PHYLIP, or NEXUS formatted files.
}

\usage{
	
write.fas(x, file = "", interleave = FALSE, truncate = FALSE, append = FALSE)

write.phy(x, file = "", interleave = FALSE, strict = FALSE)

write.nex(x, file = "", interleave = 60, taxblock = FALSE)

}

\arguments{
  \item{x}{a list or matrix of DNA sequences.}
  
  \item{file}{a character string giving the filename; if \code{file = ""}, the file is written on the standard output connection (i.e. the console).}
  
  \item{interleave}{an integer, giving the number of bases per line.}
  
  \item{truncate}{truncation of taxon names to the number of characters given as a integer, otherwise (default) taxon names will not be changed.}
  \item{append}{logical, if \code{TRUE} the sequences will be appended to \code{file} (if it exists).}
  
  \item{strict}{logical, if \code{TRUE} the names of the sequences will be truncated to 10 strings.}
  
  \item{taxblock}{logical, if \code{TRUE}, a tax block will be added to the NEXUS file.}
}

\value{None.}

\references{
Maddison, D.R., D.L. Swofford, and W.P. Maddison. 1997. NEXUS: an extensible file format for systematic information. \emph{Syst. Biol.} \bold{46}: 590-621.}
 
\author{Christoph Heibl}

\seealso{
	\code{\link{read.fas}}, \code{\link{read.phy}}, and \code{\link{read.nex}} for reading of DNA sequence files.
}

\examples{
# Example data:
# -----------------------------
data(woodmouse)
woodmouse <- woodmouse[, 1:120]

## Examples for FASTA files
## ------------------------
write.fas(woodmouse, interleave = 60)

## Examples for PHYLIP files
## ------------------------
write.phy(woodmouse, interleave = 40)

## Examples for NEXUS files
## ------------------------
# write nexus file with taxon block
write.nex(woodmouse, taxblock = TRUE)
# write non-interleaved nexus file without taxon block
write.nex(woodmouse, interleave = FALSE)

# Truncation of taxonnames:
# -------------------------
rownames(woodmouse)[15] <- "AVeeeeeeeeeeeeeeeeeryLongName"
write.fas(woodmouse, truncate = 10)

# If truncation leads to identical taxonnames,
# a warning will be issued:
# -------------------------
rownames(woodmouse)[14:15] <- "AVeeeeeeeeeeeeeeeeeryLongName"
write.fas(woodmouse, truncate = 10)
}