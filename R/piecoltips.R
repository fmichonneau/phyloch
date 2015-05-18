piecoltips <- 
function(tr, locdata, show.names = FALSE, d = 0.1, exclude = NULL){

	locdata <- locdata[match(tr$tip.label, rownames(locdata)), ]
	
	r <- get("last_plot.phylo", envir = .PlotPhyloEnv)
	nbtips <- r$Ntip
	xx <- r$xx[1:nbtips]
	xx <- xx + d
	yy <- r$yy[1:nbtips]
	
	# exclude data in piecol which is not present in the tree
	nopiecol <- which(is.na(rownames(locdata)))
	locdata <- locdata[-nopiecol, ]
	xx <- xx[-nopiecol]
	yy <- yy[-nopiecol]
	
	# exclude data which is present in the tree, but shall not be 	# shown
	if (!is.null(exclude)){
		exc <- which(rownames(locdata) == exclude)
		locdata <- locdata[-exc, ]
		xx <- xx[-exc]
		yy <- yy[-exc]
	}
	
	piecol <- rev(rainbow(n = dim(locdata)[2]))
	BOTHlabels2(XX = xx, YY = yy, pie = locdata, 
	    sel = locdata[,1], piecol = piecol, cex = 1.5, 
	    border = NA)
	if (show.names)
	text(xx, yy, rownames(locdata))		
}