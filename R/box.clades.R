box.clades <- function(phy, nodes, col, border = NA, shaded = NULL, 
	text = NULL, textcol = "black", cex = 1, rshift = 0, align, 
	expand = c(0, 0, 0, 0)){
		
	## check tree
	## ----------
	if ( !inherits(phy, "phylo") ) 
        stop("object 'phy' is not of class 'phylo'")
	ntips <- Ntip(phy)
  
	## check nodes
	## -----------
	nClades <- length(nodes)
	desNodes <- lapply(nodes, descendants, phy = phy)

	## parameters of previous call to plot.phylo
	## -----------------------------------------
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)

	## check if type is supported
	## --------------------------
	#if ( lastPP$type != "phylogram" )
	#	stop("currently only type = 'phylogram' supported")
  
	## set direction-related parameters
	## --------------------------------
	dir <- lastPP$direction
	#if ( !dir %in% c("rightwards", "leftwards") )
	#	stop("currently only directions 'rightwards' and 'leftwards' supported")
	d.sign <- ifelse(dir %in% c("rightwards", "upwards"), 1, -1)
	minmax <- ifelse(dir %in% c("rightwards", "upwards"), 2, 1)
	maxmin <- ifelse(dir %in% c("rightwards", "upwards"), 1, 2)
	crds <- c("xx", "yy")
	cid <- ifelse(dir %in% c("rightwards", "leftwards"), 1, 2)
	new.list.element <- paste(crds[cid], "new", sep = ".")
	
	if ( missing(align) ) align <- "cladewise"
	
	## space: x-coordinates of the end of the tiplabels
	## ------------------------------------------------
  if ( lastPP$show.tip.label ){
    labelSpace <- string.dim(phy$tip.label, cex = lastPP$cex, 
                             font = lastPP$font, srt = lastPP$srt)[, cid]
    labelSpace <- labelSpace + lastPP$label.offset
  } else {
    labelSpace <- rep(0, ntips)
  }
	labelSpace <- lastPP[[crds[cid]]][1:ntips] + labelSpace * d.sign
	    
	## extension: half the font height
	## -------------------------------
	extension <- ifelse(dir %in% c("rightwards", "leftwards"),
		strheight("A", cex = lastPP$cex, font = lastPP$font),
		strwidth("A", cex = lastPP$cex, font = lastPP$font)) 
	extension <- extension * .75	    

	## calculate 'vertical' extension of boxes
	## ---------------------------------------
	getCladeHeight <- function(x, extension, phy){
		x <- range(descendants(phy, x))
		if ( length(x) == 0 ) x <- x + c(-extension, extension) # single tips!
		x + c(-extension, extension)
	} 
	cladeHeight <- lapply(nodes, getCladeHeight, 
		extension = extension, phy = phy)
	cladeHeight <- do.call(rbind, cladeHeight)
	
	## calculate 'inner' margin of boxes
	## -------------------------------
	cladeMRCA <- lastPP[[crds[cid]]][nodes]
	
	## calculate 'outer' margin of boxes
	## ---------------------------------
	getCladeTips <- function(x, phy, ntips, dir){
		if ( x > ntips ) x <- descendants(phy, x)
    range(labelSpace[x])[minmax]
	} 
	cladeTips <- sapply(nodes, getCladeTips, 
		phy = phy, ntips = ntips, dir = d.sign)
	cladeTips <- cladeTips + rshift * d.sign
  if ( !is.null(text) ){
    cladeTips <- cladeTips + d.sign *
      string.dim(paste("-", text, sep = ""), cex = cex, srt = 0)[, cid]
  }
	if ( align == "all" ) {
	  mostExtremeTip <- ifelse(d.sign == 1, max(cladeTips), min(cladeTips))
	  cladeTips <- rep(mostExtremeTip, nClades)
	}
	
	## calculate coordinates for placement of boxes
	## --------------------------------------------
	if ( dir %in% c("rightwards", "leftwards") )
		xy <- cbind(cladeMRCA, cladeHeight[, 1], cladeTips, cladeHeight[, 2])
	else
		xy <- cbind(cladeHeight[, 2], cladeTips, cladeHeight[, 1], cladeMRCA)
	colnames(xy) <- c("xleft", "ybottom", "xright", "ytop")
  if ( lastPP$type == "cladogram" ) {
    xx <- cbind(cladeMRCA, 
                cladeMRCA,
                lastPP[[crds[cid]]][1],
                cladeTips,
                cladeTips,
                lastPP[[crds[cid]]][1]
              )
    yy <- cbind(rowMeans(cladeHeight) + extension,
                rowMeans(cladeHeight) - extension,
                cladeHeight[, 1], 
                cladeHeight[, 1], 
                cladeHeight[, 2], 
                cladeHeight[, 2])
  }

	# plot boxes:
	# ---------------
	if ( is.null(shaded[1]) ) {
    if ( missing(col) ) col <- "lightgrey"
    if ( length(col) < nClades )
      col <- rep(col, ceiling(nClades / length(col) ))
		#rect(xy[, 1], xy[, 2], xy[, 3], xy[, 4], col = col, border = border)
	  if ( lastPP$type == "phylogram" ){
      for ( i in 1:nrow(xy) )
        polygon(x = xy[i, c(1, 3, 3, 1)], y = xy[i, c(2, 2, 4, 4)], 
                col = col[i], border = border[i])
	  }
    if ( lastPP$type == "cladogram" ){
      for ( i in 1:nrow(xx) )
        polygon(x = xx[i, ], y = yy[i, ], 
                col = col[i], border = border[i])
    } 
	}
	else {
		n <- 1000
		scol <- rev(sequential_hcl(
			n, 
			h = shaded$h, 
			c. = shaded$c, 		
			l = shaded$l, 
			power = shaded$power)
		)	
		for ( i in 1:nrow(xy) ){
			xvect <- seq(from = xy[i, 1], to  = xy[i, 3], 
				by = (xy[i, 3] - xy[i, 1]) / n)
			rect(xleft = xvect, 
				ybottom = xy[i, 2], 
				xright = xvect + (xy[i, 3] - xy[i, 1]) / n, 
				ytop = xy[i, 4], col = scol, border = border)
		}	
	}

	# plot text:
	# ----------
	if ( !is.null(text) )
		text(x = xy[, 3], y = apply(xy[, c(2, 4)], 1, mean), 
         text, cex = cex, pos = c(4, 2)[minmax], col = textcol)
  
	# update and return 'last_plot.phylo'
	# -----------------------------------
  new <- lastPP[[crds[cid]]]
  for ( i in seq_along(desNodes) ) {
    new[desNodes[[i]]] <- cladeTips[i]
  }
	lastPP[[new.list.element]] <- new
	assign("last_plot.phylo", lastPP, envir = .PlotPhyloEnv)
	invisible(lastPP)
}
