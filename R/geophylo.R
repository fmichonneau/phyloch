geophylo <- function(phy, file = "", loc, z_exag = 1000){
	
	ntips <- length(phy$tip.label)
	nnodes <- phy$Nnode
	intnodes <- nnodes:1 + ntips
	
	# node heights + tipheights: z
	# ----------------------------
	nodeheights <- branching.times(phy)
	tipheights <- rep(0, ntips)
	names(tipheights) <- 1:ntips
	z <- c(tipheights, nodeheights)
	z <- round(z * z_exag) # superelevation
	
	# arrange 'loc':
	# ---------------------------------
	ind <- which(rownames(loc) %in% phy$tip.label)
	loc <- loc[ind, ]
	loc <- loc[match(phy$tip.label, rownames(loc)), ]
		
	# finding arithmetic mean between two coordinates
	# -----------------------------------------------
	midpoint <- function(x1, y1, x2, y2){
		x <- sort(c(x1, x2), decreasing = TRUE)
		y <- sort(c(y1, y2), decreasing = TRUE)
	
		x <- x[1] - (x[1] - x[2]) / 2
		y <- y[1] - (y[1] - y[2]) / 2
		out <- c(x, y)
		out	
	}
	
	rownames(loc) <- 1:ntips
	colnames(loc) <- c("x", "y")
	loc <- rbind(loc, matrix(nrow = nnodes, ncol = 2))
	for (i in intnodes){
		d <- phy$edge[phy$edge[,1] == i, 2]
		m <- midpoint(loc[d[1], 1], loc[d[1], 2],
			loc[d[2], 1], loc[d[2], 2])
		loc[i, ] <- m
		rownames(loc)[i] <- i
	}
	
	# coords
	loc <- cbind(loc, z)
	
	# assemble ASCII file
	# ---------------------
	L1 <- "L 2"
	for (i in seq(along = phy$edge[, 1]))
	
	foo <- function(x, loc){
		o <- vector(length = 3)
		o[1] <- "L 2"
		o[2] <- paste(loc[x[1],], collapse = " ")
		o[3] <- paste(loc[x[2],], collapse = " ")
		o
	}
	dd <- apply(phy$edge, 1, foo, loc = loc)
	
	if (file == "") return(dd)
	else write(dd, file = file)	
}
