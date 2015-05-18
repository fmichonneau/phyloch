strwidth90 <- function(s, cex = NULL, font = NULL){
  
  if ( is.null(cex) ) cex <- par("cex")
  if ( is.null(font) ) font <- par("font")
  xusr <- par("usr")
  xh <- strwidth(s, cex = cex, font = font)
  yh <- strheight(s, cex = cex, font = font)
  xh <- yh / (xusr[4] - xusr[3]) * par("pin")[2]
  xh <- xh / par("pin")[1] * (xusr[2] - xusr[1])
  return(xh)
}
