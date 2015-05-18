compare.phylo <- function(x, y, ts = "arrow", threshold, presCol = "green", absCol = 
    "red", tsCol = "blue", pch, pcex, tipcex, plottype, ...){
    	
    ## CHECKS and DEFINITIONS
    ## ----------------------
    if (!inherits(x, "phylo")) 
        stop("object 'x' is not of class 'phylo'")
    if (!inherits(y, "phylo")) 
        stop("object 'y' is not of class 'phylo'")
    if (missing(pcex)) pcex <- 1
    if (missing(tipcex)) tipcex <- 1
    if (missing(plottype)) plottype <- "visible"
		
	# delete tips not contained in both trees
	# ---------------------------------------
	missingINy <- setdiff(x$tip.label, y$tip.label)
	cat("\n", paste(length(missingINy), 
		" tips from 'x' are missing in 'y'\n\t", 				
		paste(missingINy, collapse = "\n\t"), sep = ""), sep = "")
	missingINx <- setdiff(y$tip.label, x$tip.label)
	cat(paste("\n", length(missingINx), 
		" tips from 'y' are missing in 'x'\n\t", 				
		paste(missingINx, collapse = "\n\t"), "\n", sep = ""))
	if ( length(missingINy) > 0 )
		x <- drop.tip2(x, missingINy)
	if ( length(missingINx) > 0 )
		y <- drop.tip2(y, missingINx)
	
	# plot phylogeny 'x'
	# ------------------										
	if (plottype == "visible") plot(x, cex = tipcex, ...)
	if (plottype == "invisible") plot(x, tip.color = 0, edge.color = 0,
	     cex = tipcex, ...)
	if (plottype == "upon") plot.phylo.upon(x, cex = tipcex, ...)
	
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
				
	# xnodes: vector of internal nodes
	# --------------------------------
	xnodes <- lastPP$Ntip + 1:lastPP$Nnode
	
	# dd: nodes that 'x' and 'y' have in common
	# -----------------------------------------------
	set <- lapply(xnodes, phyloch::descendants, phy = x, type = "t", labels = TRUE)
	ynodes <- noi(phy = y, group = set)
	inodes <- rbind(xnodes, ynodes)
	td <- as.numeric(names(table(ynodes))[table(ynodes) > 1])
	dd <- rep(TRUE, length(ynodes))
	for (i in td)
		dd[tail(which(ynodes == i), -1)] <- FALSE
	
	# colors
	# ------
	col <- rep(absCol, length(dd))
	col[dd] <- presCol
	if (missing(pch)) pch <- 21
	if (plottype != "invisible")
	    nodelabels(pch = pch, bg = col, cex = pcex, ...)	
	
	# calculate and visualize time shift:
	# -----------------------------------
	ultmtc <- c(is.ultrametric(x), is.ultrametric(y))
	if ( all(ultmtc) ){
		snodes <- inodes[, dd] #shared nodes
		maxage <- max(lastPP$xx)
		if ( missing(threshold) ) threshold <- maxage * .01
		xtimes <- branching.times(x)
		ytimes <- branching.times(y)
		names(xtimes) <- names(ytimes) <- inodes[1, ]
	
		timeshift <- vector()
		for (i in seq(along = snodes[1, ])){
			id1 <- snodes[1, i]
			id2 <- snodes[2, i]
			nn <- ytimes[names(ytimes) == id2]
			tts <- lastPP$xx[id1] - (maxage - nn) #thistimeshift
			timeshift <- c(timeshift, tts)
			if (ts == "arrow" & plottype != "invisible"){
				if (abs(tts) >= threshold)
					arrows(x0 = lastPP$xx[id1], 
						y0 = lastPP$yy[id1], 
						x1 = maxage - nn, 
						length = 0.1, 
						col = tsCol, ...)
			} # end of ts == "arrow"
		} # end of loop over snodes
		# plot time shift as real number
		# ------------------------------
	    if (ts == "numbers" & plottype != "invisible"){
		    rts = round(timeshift, 2) # rounded time shift
		    pncol <- rep("lightblue", length(rts))
		    pncol[rts < 0] <- "lightpink"
		    nodelabels(text = rts, node = snodes[1,], cex = tipcex,
			    bg = pncol)
	    }
	    # print summary statistics:
	    # -------------------------
	    snodes <- rbind(snodes, timeshift)
	    cat("\nSummary of the time shift of internal nodes:")
	    cat("\n max:", max(snodes[3,]))
	    cat("\n min:", min(snodes[3,]))
	    cat("\n median:", median(snodes[3,]))
	    cat("\n mean:", mean(snodes[3,]))
	    cat("\n standard diviation:", sd(snodes[3,]), "\n\n")
	    	
	} # end of "both trees are ultrametric"
	else {
		w <- paste(c("'x'", "'y'")[ultmtc], collapse = ", ")
		w <- paste(w, "is not ultrametric: no time shift calculated")
		if (all(ultmtc)) w <- gsub("is", "are", w)
		warning(w)
		snodes <- w
	}
	invisible(list(x = x, y = y, shared_nodes = snodes))	
}
