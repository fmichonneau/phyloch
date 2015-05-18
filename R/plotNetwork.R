plotNetwork <- 
function(m, freq, spread = 180, rotate = 0, mirror = FALSE, freqmode = 2, loc = NULL, locorder = "lat", cex = 1, col = "r", piecolrev = FALSE, contrast = 0,  border = NA, xshift = 0, yshift = 0, replace.names = FALSE, stp = NA, modify = NULL){

	# definitions:
	# ------------
	if (missing(freq)) {
		freq <- rep(1, dim(m)[1])
		names(freq) <- rownames(m)
	}
	if (replace.names){
		names <- rownames(m)
		new.names <- LETTERS[seq(along = names)]
		names(freq) <- rownames(m) <- colnames(m) <- new.names
		
	}
	
	
	# identify 'cn' (= central node):
	# the node with the greatest number 
	# of branches connected to it (nbc) ...
	# _____________________________________
	nbc <- apply(m, 1, sum)
	cn <- which(nbc == max(nbc))[1] 
	# ... and start matrix 'xy'
	# -------------------------
	xy <- matrix(c(cn, 0, 0, 0, 0), nrow = 1)
	rownames(xy) <- rownames(m)[cn]	 
	# 'dn': identify neighboring nodes of cn
	# --------------------------------------
	dn <- rn <- angle <- which(m[cn, ] == 1)	
	# 'angle': calculate angle of edges in degree ...
	# -----------------------------------------------
	nb.dn <- length(dn)
	angle[1] <- rotate
	for (i in 2:nb.dn)
		angle[i] <- angle[i-1] + 360 / nb.dn
	# ... and 
	rad <- angle * (pi / 180) # im Bogenmaß: Vollkreis = 2*pi
	
	# adjust edges for differences in point size
	# ------------------------------------------
    inner <- sqrt(rep(freq[cn], nb.dn))
	outer <- sqrt(freq[dn])
	x <- sin(rad) + sin(rad) * inner + sin(rad) * outer
	y <- cos(rad) + cos(rad) * inner + cos(rad) * outer
	# ----------------------
	angles <- cbind(dn, x, y, angle, rad)
	angles[abs(angles) < 0.000001] <- 0
	xy <- rbind(xy, angles)
	# 'edge': matrix of connected nodes
	# ---------------------------------
	edge <- cbind(rep(cn,  nb.dn), dn)
	
	# all other nodes ....
	# --------------------
	step <- 1
	while (length(rn) > 0){
		if (length(spread) > 1)
			thisspread <- spread[step]                                       else thisspread <- spread
	    
		# first item of set of next step nodes is the next node 
		# to connect
		# _____________________________________________________
		cn <- rn[1]
		rn <- rn[-1]
		dn <- which(m[cn, ] == 1)
		dn  <- dn[!dn %in% xy[, 1]] # suppresses loops!!!
		nb.dn <- length(dn)			
		edge <- rbind(edge, cbind(rep(cn,  nb.dn), dn))
		
		# order descending nodes manually
		# -------------------------------
		if (is.list(modify)){
			if (step == as.numeric(gsub("s", "", names(modify)[1]))){
				dn <- dn[modify[[1]]]
				if (length(modify) > 1)
					modify <- modify[-1]
			}
		}
				
		if (nb.dn > 0){
			angle <- divide.angle(nb.dn, thisspread)
			stan <- xy[xy[,1] == cn, 4]
			angle <- angle + stan
			rad <- angle * (pi / 180)
			x.ac <- xy[xy[,1] == cn,2]
			y.ac <- xy[xy[,1] == cn,3]
			inner <- sqrt(rep(freq[cn], nb.dn))
	        outer <- sqrt(freq[dn])
			x <- x.ac + sin(rad)+ sin(rad) * inner + sin(rad) * outer
			y <- y.ac + cos(rad)+ cos(rad) * inner + cos(rad) * outer
			angles <- cbind(dn, x, y, angle, rad)
			angles[abs(angles) < 0.000001] <- 0
			xy <- rbind(xy, angles)
			rn <- c(dn, rn)				
		}
		step <- step + 1
	} # end of loop
	
	
	if (mirror)
		xy[,2] <- -xy[, 2]
	
	#	PLOTTING:
	# ________________________________________________________
	
	x1 <- min(xy[,2])
	x2 <- max(xy[,2])
	y1 <- min(xy[,3])
	y2 <- max(xy[,3])
	dd <- c(x1, x2, y1, y2)
	dd <- c(min(dd), max(dd))
	
	# center haplotype network
	# ________________________
	#width <- dd[2] - dd[1]
	xy[, 2] <- xy[, 2] + xshift
	xy[, 3] <- xy[, 3] + yshift
	
	par(mar = c(0,0,0,0))
	plot(xy.coords(dd*1.1, dd*1.1), type = "n", axes = FALSE, 
	    xlab = " ", ylab = " ")
	#axis(1)
	#axis(2)
	    
	# manage point sizes
	# ------------------
	freq <- freq[match(rownames(xy), names(freq))]
	cex <- rep(cex, length = dim(xy)[1])
	pcex <- cex * 3 * sqrt(freq) # factor 3 to adjust to tcex!
	tcex <- cex * sqrt(freq)
	
	# manage point colors
	# -------------------
	pcol <- rep("black", length = dim(xy)[1])
	tcol <- rep("white",  length = dim(xy)[1])
		
	#observed <- which(!rownames(xy) %in% unobserved)
	observed <- seq(along = rownames(xy))
	
	col <- match.arg(col, c("rainbow", "heat.color", 		"terrain.colors", "topo.colors", "cm.colors"))
	
		if (!is.null(loc)) {
		# order locdata according to xy
		locdata <- locdata[match(rownames(xy)[observed], 		               
		    rownames(locdata)), ]
		nb.colors <- dim(locdata)[2] + contrast
		if (col == "rainbow")
			piecol <- rainbow(n = nb.colors)
		if (col == "heat.color")
			piecol <- heat.colors(n = nb.colors)
		if (col == "terrain.colors")
			piecol <- terrain.colors(n = nb.colors)
		if (col == "topo.colors")
			piecol <- topo.colors(n = nb.colors)
		if (col == "cm.colors")
			piecol <- cm.colors(n = nb.colors)
		piecol <- piecol[1:dim(locdata)[2]]
		tcol[observed] <- "black"
	}												else {
		nb.colors <- length(observed)
		if (col == "rainbow")
		pcol[observed] <- rainbow(n = nb.colors)
		if (col == "heat.color")
		pcol[observed] <- heat.colors(n = nb.colors)
		if (col == "terrain.colors")
		pcol[observed] <- terrain.colors(n = nb.colors)
		if (col == "topo.colors")
		pcol[observed] <- topo.colors(n = nb.colors)
		if (col == "cm.colors")
		pcol[observed] <- cm.colors(n = nb.colors)
		tcol[observed] <- "black"
	}
	
	if (piecolrev) piecol <- rev(piecol)
	
	# print edges:
	# _____________________________________
	for (i in 1:(dim(edge)[1])){
		# x coordinate
		node1 <- which(xy[,1] == edge[i,1])
		# y coordinate 			
		node2 <- which(xy[,1] == edge[i,2]) 
		lines(c(xy[node1, 2],xy[node2, 2]), 
		    c(xy[node1, 3],xy[node2, 3]))
	}
			
	# print observed haplotypes
    # ---------------------------------------
	if (is.null(loc))
		points(xy[observed, 2], xy[observed,3], pch = 21, 			
		cex = pcex[observed], bg = pcol[observed], col = border)		else {
		BOTHlabels2(XX = xy[observed, 2], YY = xy[observed,3], 			        pie = locdata, sel = locdata[,1], piecol = piecol, 
		    cex = pcex[observed] / 3, border = NA)
		points(xy[observed, 2], xy[observed,3], pch = 21, 			       
		    cex = pcex[observed], bg = NULL, col = border)
	}	
	
	# print unobserved haplotypes = mutations
	# ---------------------------------------
	points(xy[-observed, 2], xy[-observed,3], pch = 21, cex = pcex		[-observed], bg = pcol[-observed])
	
	# print labels
	# ------------
	text(xy[observed,2], xy[observed,3], rownames(xy)[observed], 		col = tcol[observed], cex = tcex[observed])
	
	#lines(c(1, 1, -1, -1, 1), c(1, -1, -1, 1, 1))
	
	loc	
}
