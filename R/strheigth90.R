strheight90 <- function(s, cex = NULL, font = NULL){
  
  if ( is.null(cex) ) cex <- par("cex")
  if ( is.null(font) ) font <- par("font")
  xusr <- par("usr")
  xh <- strwidth(s, cex = cex, font = font)
  yh <- strheight(s, cex = cex, font = font)
  yh <- xh / (xusr[2] - xusr[1]) * par("pin")[1]
  yh <- yh / par("pin")[2] * (xusr[4] - xusr[3])
  return(yh)
}