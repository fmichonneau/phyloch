broken.segments <- 
function(x0, y0, x1 = x0, y1 = y0, brokeat = 0.15, gap = 0.02,
   tilt = 45, scale = 0.5, lwd = 1, ...){
   	
	gap <- gap * lwd
	
	if (x1 > x0)
		x3 <- x1
	else{
		x3 <- x0
		x0 <- x1
		y <- y0
		y0 <- y1
		y1 <- y
	}
	
	l <- x3 - x0
	brokeat <- l * brokeat
	gap <- l * gap
	x1 <- x0 + brokeat - gap/2
	x2 <- x0 + brokeat + gap/2
	
	segments(x0, y0, x1, y1, lwd = lwd, ...)
	segments(x2, y0, x3, y1, lwd = lwd, ...)
	
	x1 <- x1 + gap * 0.15
	x2 <- x2 - gap * 0.15
	gap <- c(-gap, gap) * scale
	tilt <- tilt * pi / 180
	lines(x = x1 + gap * tilt, y = y0 + gap, 
		lwd = lwd * 0.6, ...)
	lines(x = x2 + gap * tilt, y = y0 + gap, 
		lwd = lwd * 0.6, ...)	
}