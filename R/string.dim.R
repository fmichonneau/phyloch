string.dim <- function(s, cex = NULL, font = NULL, srt = 0){
   
  if ( is.null(cex) ) cex <- par("cex")
  if ( is.null(font) ) font <- par("font")
  
  x <- strwidth(s, cex = cex, font = font)
  y <- strheight(s, cex = cex, font = font)
  xy <- cbind(x, y)
  
  if ( srt != 0 )  {
    ## usr: extremes of the user coordinates
    xusr <- par("usr")
    x.usr <- xusr[2] - xusr[1]
    y.usr <- xusr[4] - xusr[3]
    
    ## pin: current plot dimension in inches
    x.inch <- par("pin")[1]
    y.inch <- par("pin")[2]
   
    #s calculate convertion factors and ... 
    h2v <- ( x.inch * y.usr ) / ( x.usr * y.inch )
    v2h <- ( y.inch * x.usr ) / ( y.usr * x.inch )
    ## ... do conversion
    xy <- cbind(xy[, "y"] * v2h, xy[, "x"] * h2v)
  }
  colnames(xy) <- c("x", "y")
  return(xy)
}
