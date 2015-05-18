add.timeframe <- 
function(phy, age, clade = NULL, tips = NULL, ...){

	# get coordinates and number of tip labels of
	# preceeding 'plot.phylo' call
	# ----------------------------
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
	ntips <- lastPP$Ntip
	rootage <- branching.times(phy)[1]
	
	# clade
	if (!is.null(clade)){
		extension <- lastPP$cex/2
		d <- head(descendants(phy, clade), 1) - extension
		dd <- tail(descendants(phy, clade), 1) + extension
		yy <- sort(c(d, dd))
	}												
	else {
		if (is.null(tips))
			yy <- c(1, ntips)							
		else yy <- tips
		# adjust overshoot
		# ----------------
		yext <- 0.01 * ntips
		yy[1] <- yy[1] - yext # ybottom
		yy[2] <- yy[2] + yext # ytop
	}
	
	# plot line ...
	if (length(age) == 1){
		xx <- rep(rootage - age, 2)
		lines(xx, yy, ...)
	}
	# or rectangle
	else { 
		xleft <- rootage - max(age)
		xright <- rootage - min(age)
		rect(xleft, yy[1], xright, yy[2], ...)
	}
}
