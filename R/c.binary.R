`c.binary` <- 
function(..., match = TRUE){
	
	x <- list(...)
	x1 <- x[[1]]
	x2 <- x[[2]]
	names1 <- rownames(x1)
	names2 <- rownames(x2)
	matching <- names1[names1 %in% names2]
	notin1 <- names2[!names2 %in% names1]
	notin2 <- names1[!names1 %in% names2]
	
	
	# fuse matching
	# ======================================================
	if (length(matching) == 1) {
		y1 <- x1[rownames(x1) %in% matching, ]
		y2 <- x2[rownames(x2) %in% matching, ]
		y <- c(y1, y2)
		y <- rbind(y, y)
		rownames(y) <- c(matching, "DUMMY")
	}												else {
		y1 <- x1[rownames(x1) %in% matching, ]
		y2 <- x2[rownames(x2) %in% matching, ]
		y2 <- y2[match(rownames(y2), rownames(y1)), ]
		y <- cbind(y1, y2)
		
	}
	
	
	# fuse non-matching
	# ======================================================
	if (!match){
		# sequences that are not in 1
		if (length(notin1) > 1){
			nbtax <- length(notin1)
			nbcha <- dim(x1)[2]
			ns <- matrix("N", nrow = nbtax, ncol = nbcha)
			x22 <- x2[rownames(x2) %in% notin1, ]
			rownames(ns) <- rownames(x22)
			yy <- cbind(ns, x22)
			y <- rbind(y, yy)
		}
		if (length(notin1) == 1){
			nbtax <- length(notin1)
			nbcha <- dim(x1)[2]
			ns <- matrix("N", nrow = nbtax, ncol = nbcha)
			x22 <- x2[rownames(x2) %in% notin1, ]
			yy <- c(ns, x22)
			y <- rbind(y, yy)
			rownames(y)[rownames(y) == "yy"] <- notin1
		}

		# sequences that are not in 2
		if (length(notin2) > 1){
			nbtax <- length(notin2)
			nbcha <- dim(x2)[2]
			ns <- matrix("N", nrow = nbtax, ncol = nbcha)
			x11 <- x1[rownames(x1) %in% notin2, ]
			rownames(ns) <- rownames(x11)
			yy <- cbind(x11, ns)
			y <- rbind(y, yy)
		}
		if (length(notin2) == 1){
			nbtax <- length(notin2)
			nbcha <- dim(x2)[2]
			ns <- matrix("N", nrow = nbtax, ncol = nbcha)
			x11 <- x1[rownames(x1) %in% notin2, ]
			yy <- c(x11, ns)
			y <- rbind(y, yy)
			rownames(y)[rownames(y) == "yy"] <- notin2
		}
	}
	
	# DELETE DUMMY ROWS
	ind <- which(rownames(y) %in% "DUMMY")
	if (length(ind) > 0)
		y <- y[-ind, ]
	y
}