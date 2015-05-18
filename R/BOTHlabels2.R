BOTHlabels2 <- 
function (sel, XX, YY, pie, piecol, cex, border) 
{
	if (is.vector(pie)) 
    	pie <- cbind(pie, 1 - pie)
    xrad <- cex * diff(par("usr")[1:2])/50
    xrad <- rep(xrad, length(sel))
    for (i in seq(along = pie[, 1])) 
    	#locpie(XX[i], YY[i], pie[i, ], radius = xrad[i], 
    		#col = piecol, border = border)
    	#floating.pie.asp
    	locpie(xpos = XX[i], ypos = YY[i], 
    		x = pie[i, ], edges = 200, radius = cex/2, 
    		col = piecol, startpos = pi/2, border = border) 
}