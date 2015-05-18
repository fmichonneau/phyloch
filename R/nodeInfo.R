nodeInfo <- 
function(node, label, label2 = NULL, cex = 1, angle = 225, 
    col = "red", lex = 1){
	
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
	
	# ratio between x and y axis is not equal in phylo plots
	# ------------------------------------------------------
	ratio <- (max(lastPP$xx) - min(lastPP$xx)) /
		(max(lastPP$yy) - min(lastPP$yy))
		
	# highlight node
	# --------------
	x <- lastPP$xx[node]
	y <- lastPP$yy[node]
	points(x, y, pch = 1, cex = 2, col = col)
	
	# plot line/arrow
	# ---------------
	if (is.null(angle)){
		cat("\nClick on plot to place the text.\n")
		xy <- locator(1)
		x <- c(x, xy$x) 
		y <- c(y, xy$y)
	}												else {
		rad <- 2 * pi * angle / 360
		x <- c(x, (x + cos(rad) * ratio * lex)) 
		y <- c(y, (y + sin(rad) * lex))
	}
	lines(x, y, col = col)
	
	# plot text frame
	# ---------------
	horiz <- 0.5 * strwidth(label, cex = cex) * 1.1
	if (is.null(label2)) verti <- 0.5 * strheight(label, cex = cex) * 1.5 else verti <- 0.5 * strheight(label, cex = cex)* 2.3 * 1.5
	xleft <- x[2] - horiz
	ybottom <- y[2] - verti
	xright <- x[2] + horiz
	ytop <- y[2] + verti
	rect(xleft, ybottom, xright, ytop, col = "white", 		
	   border = col, xpd = NA)
	
	# plot text
	# ---------
	if (is.null(label2)) 
	    text(x[2], y[2], label, cex = cex, col = col, xpd = NA)
	else {
		text(x[2], y[2], adj = c(0.5, 0), label, cex = cex, col = col,
		    xpd = NA)
	    text(x[2], y[2] - strheight(label2, cex = cex),
	        labels = label2, cex = cex, col = col, xpd = NA)
	}
}
