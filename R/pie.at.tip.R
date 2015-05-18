pie.at.tip <- 
function (tips, pie, piecol, border, cex, offset) 
{
	sel <- 100
	
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
	if (missing(tips)) tips <- 1:lastPP$Ntip
	
	# get coordinates of tips
	# -----------------------
	XX <- lastPP$xx[1:lastPP$Ntip]
	YY <- lastPP$yy[1:lastPP$Ntip]
	
	# manage offset
	# -------------
	if (lastPP$type == "phylogram")
		XX <- XX + offset
	if (lastPP$type == "fan"){
		a <- (360 / lastPP$Ntip) * tips
		a <- a * (pi / 180)
		XX <- XX + offset * cos(a)
		YY <- YY + offset * sin(a)	
	}
	
	if (is.vector(pie)) 
    	pie <- cbind(pie, 1 - pie)
    xrad <- cex * diff(par("usr")[1:2])/50
    xrad <- rep(xrad, length(sel))
    for (i in seq(along = tips)) {
    	#locpie(XX[i], YY[i], pie[i, ], radius = xrad[i], 
    		#col = piecol, border = border)
    	#floating.pie.asp
    	locpie(xpos = XX[i], ypos = YY[i], 
    		x = pie[i, ], edges = 200, radius = cex/2, 
    		col = piecol, startpos = pi/2, border = border) 
    }
}