add.trendline <- 
function(phy, x, axis = TRUE, ...){

	# get coordinates and number of tip labels of
	# preceeding 'plot.phylo' call
	# ----------------------------
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
	ntips <- lastPP$Ntip
	rootage <- branching.times(phy)[1]
	
	# rescale x ...
	x[, 1] <- rootage - x[, 1]
	
	# rescale y ...
	maxploty <- max(lastPP$y.lim)
	maxdatay <- max(x[, 2])
	yvals <- x[, 2] # for axis labels	
	x[, 2] <- x[, 2] * maxploty / maxdatay 
	
	# plot trendline
	lines(x, ...)
	
	# add axix:
	if (axis){
		lab <- pretty(yvals)
		at <- lab * maxploty / maxdatay
		axis(side = 2, at = at, labels = lab)
	}	
}
