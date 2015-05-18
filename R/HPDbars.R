HPDbars <- 
function(phy, label = "height_95%_HPD", tab = NULL, nodes, col, lwd, broken = FALSE, ...){
	
	# check input data
	# ----------------
	if (!inherits(phy, "phylo")) 
        stop("object 'phy' is not of class 'phylo'")
    if (!label %in% gsub("_MIN|_MAX", "", names(phy))) 
        stop("there is no element '", label, "' in object 'phy'")
    if (!paste(label, "MIN", sep = "_") %in% names(phy)) 
        stop("there is no lower bound for '", label, 
            "' in 'phy': the corresponding element must be named '", 			label, "_MIN'")
    if (!paste(label, "MAX", sep = "_") %in% names(phy)) 
        stop("there is no upper bound for '", label, 
            "' in 'phy': the corresponding element must be named '", 			label, "_MAX'")
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
	if (!lastPP$use.edge.length)
	    stop("function needs edge length information")
	if (lastPP$type != "phylogram")
	    stop("currently only 'type == phylogram' supported")
	
	# set default parameters
	# ----------------------
	if (missing(lwd)) lwd <- 5
	if (missing(col)) col <- "grey75"
	
	if (missing(nodes)) nodes <- lastPP$Ntip + 1:lastPP$Nnode
	nodes <- sort(nodes)
	
	# get x and y values:
	# -------------------
	label <- paste(label, c("MIN", "MAX"), sep = "_")
	if (is.null(tab)){
		id <- which(names(phy) %in% label)
		x <- cbind(phy[[id[1]]], phy[[id[2]]])
		x <- x[nodes - lastPP$Ntip, ]
	}												else {
		id <- which(colnames(tab) %in% label)
		x <- tab[tab[, 1] %in% nodes, id]
	}
	
	bt <- branching.times(phy)
	bt <- bt[names(bt) %in% nodes]
	x <- cbind(x, bt)
	x <- cbind(x[, 3] - x[, 1], x[, 2] - x[, 3], x)
	colnames(x) <- c("min", "max", "MIN", "MAX", "mean")
	
	if (lastPP$dir %in% c("upwards", "downwards")){
		y <- x
		x <- lastPP$xx[nodes]
	}
	else y <- lastPP$yy[nodes]
	
	# plot bars:
	# -------------
	if (lastPP$dir == "rightwards"){
		xx <- lastPP$xx[nodes]
		xx <- cbind(xx - x[, 2], xx + x[, 1])
		id <- which(xx[, 1] < lastPP$x.lim[1])
		if (length(id) > 0) {
			ns <- paste(nodes[id], collapse = ", ")
			minx <- min(xx[id, 1])
			new.xlim <- c(minx, lastPP$x.lim[2] - minx)
			new.xlim <- round(new.xlim, digits = 5)
			new.xlim <- paste(new.xlim, collapse = ", ")
			warning("HPD bar(s) for nodes ", ns, 
			  " exceed(s) plot region.", 
			  "\n  Try setting \"x.lim\" to c(", new.xlim, ")")
			segments(xx[id, 1], y[id], 
			    lastPP$xx[lastPP$Ntip + id], y[id], 
		        col = col, lwd = lwd, lend = 1, lty = "11")
		    segments(lastPP$xx[lastPP$Ntip + id], y[id], 
			    xx[id, 2], y[id], 
		        col = col, lwd = lwd, lend = 0)
		    xx <- xx[-id, ]; y <- y[-id]
		}
		segments(xx[, 1], y, xx[, 2], y, 
		    col = col, lwd = lwd, ...)
    }
    if (lastPP$dir == "leftwards"){
    	xx <- lastPP$xx[nodes]
    	xx <- cbind(xx - x[, 1], xx + x[, 2])
		id <- which(xx[, 2] > lastPP$x.lim[2])
		if (length(id) > 0) {
			ns <- paste(nodes[id], collapse = ", ")
			warning("HPD bar(s) for nodes ", ns, 
			  " exceed(s) plot region (issue currently unsolved)")
		}
	    segments(xx[, 1], y, xx[, 2], y, 
		    col = col, lwd = lwd, ...)
	}
	if (lastPP$dir == "upwards"){
		yy <- lastPP$yy[nodes]
		yy <- cbind(yy - y[, 2], yy + y[, 1])
		id <- which(yy[, 1] < lastPP$y.lim[1])
		if (length(id) > 0) {
			ns <- paste(nodes[id], collapse = ", ")
			miny <- min(yy[id, 1])
			new.ylim <- c(miny, lastPP$y.lim[2] - miny)
			new.ylim <- round(new.ylim, digits = 5)
			new.ylim <- paste(new.ylim, collapse = ", ")
			warning("HPD bar(s) for nodes ", ns, 
			  " exceed(s) plot region.", 
			  "\n  Try setting \"y.lim\" to c(", new.ylim, ")")
		    segments(x[id], yy[id, 1],
			    x[id], lastPP$yy[lastPP$Ntip + id],  
		        col = col, lwd = lwd, lend = 1, lty = "11")
		    segments(x[id], lastPP$yy[lastPP$Ntip + id],  
			    x[id], yy[id, 2], 
		        col = col, lwd = lwd, lend = 0)
		    yy <- yy[-id, ]; x <- x[-id]
		}
	    segments(x, yy[, 1], x, yy[, 2], 
	        col = col, lwd = lwd,  ...)
	}
	if (lastPP$dir == "downwards"){
		yy <- lastPP$yy[nodes]
		yy <- cbind(yy - y[, 1], yy + y[, 2])
		id <- which(yy[, 2] > lastPP$y.lim[2])
		if (length(id) > 0){
			ns <- paste(nodes[id], collapse = ", ")
			warning("HPD bar(s) for node(s) ", ns, 
			  " exceed(s) plot region (issue currently unsolved)")
			
		}
        segments(x, yy[, 1], x, yy[, 2], 
            col = col, lwd = lwd,  ...)
	}
}
