\name{string.dim}
\alias{string.dim}
% TITLE
\title{Dimensions of Rotated Strings}
% DESCRIPTION
\description{
Get width and height of (un-)rotated character strings for plotting with \code{\link{text}}, etc.
}
% USAGE
\usage{
string.dim(s, cex = NULL, font = NULL, srt = 0)
}
% ARGUMENTS
\arguments{
  \item{s}{a character or \code{\link{expression}} vector whose dimensions are to be determined. Other objects are coerced by \code{\link{as.graphicsAnnot}}.
}
  \item{cex}{numeric \bold{c}haracter \bold{ex}pansion factor; multiplied by \code{\link{par}("cex")} yields the final character size; the default \code{NULL} is equivalent to \code{1}.
}
  \item{font}{the font to be used: see \code{\link{par}}.
}
  \item{srt}{numeric, the string rotation in degrees.
}
}
%\details{
%-.
%}
\value{
a matrix with two named columns: \code{x} and \code{y} represent the width and the height of \code{s} in \code{"user"} units and correspond to the output of \code{strwidth} and \code{strheight} for \bold{un}rotated plotting.
}
\references{
The function is based on an e-mail exchange between Stephane Dray and Brian Ripley in November 2007: \url{http://tolstoy.newcastle.edu.au/R/e3/help/07/11/3149.html}
}
\author{
Christoph Heibl
}
\note{
Currently, \code{string.dim} can handle only horizontally (i.e. \code{srt \%in\% c(0, 180)}) and vertically (i.e. \code{srt \%in\% c(90, 270)}) plotted strings.
}
\seealso{
\code{\link{text}}, \code{\link{strwidth}}, \code{\link{strheight}}
}
\examples{
s <- "Axototl"
plot(x = c(0, 1), y = c(0, 2))

## horizontal
x <- .1; y <- .3
points(x, y, pch = 4, col = "red")
text(x, y, s, cex = 3, adj = c(0,0 ))
xy <- string.dim(s, cex = 3, srt = 0)
points(x + xy[, "x"], y, pch = 4, col = "blue")
points(x, y + xy[, "y"], pch = 4, col = "green")

## vertical
## remark: if using srt, adj still refers to the 
## dimensions like it did in the unrotated string
x <- .1; y <- .5
points(x, y, pch = 4, col = "red")
text(x, y, s, cex = 3, adj = c(0,1), srt = 90)
xy <- string.dim(s, cex = 3, srt = 90)
points(x + xy[, "x"], y, pch = 4, col = "blue")
points(x, y + xy[, "y"], pch = 4, col = "green")

## diagonal: does not yet work
x <- .4; y <- .7
points(x, y, pch = 4, col = "red")
text(x, y, s, cex = 3, adj = c(0,1), srt = 45)
xy <- string.dim(s, cex = 3, srt = 45)
points(x + xy[, "x"], y, pch = 4, col = "blue")
points(x , y + xy[, "y"], pch = 4, col = "green")
}
