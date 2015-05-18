c.genes <- function(..., single.list, match = TRUE){
	
	c.two.genes <- 
	function(x1, x2) {
		names1 <- rownames(x1)
		names2 <- rownames(x2)
		matching <- names1[names1 %in% names2]
		if ( length(matching) == 0 ) {
		  #stop("At least one matching pair of taxon names required!")
		  n1 <- as.DNAbin(matrix("n", nrow(x2), ncol(x1)))
		  rownames(n1) <- rownames(x2)
		  n2 <- as.DNAbin(matrix("n", nrow(x1), ncol(x2)))
		  rownames(n2) <- rownames(x1)
		  y <- cbind(rbind(x1, n1), rbind(n2, x2), check.names = FALSE)
		}
		else {
		  notin1 <- names2[!names2 %in% names1]
		  notin2 <- names1[!names1 %in% names2]
		  
		  # fuse matching
		  # -------------
		  y1 <- x1[rownames(x1) %in% matching, ]
		  y2 <- x2[rownames(x2) %in% matching, ]
		  y2 <- y2[match(rownames(y2), rownames(y1)), ]
		  y <- cbind(y1, y2)
		  if ( match ) y <- delete.empty.cells(y)
		  
		  # fuse non-matching
		  # -----------------
		  if ( !match ){
		    # sequences that are not in 1
		    if (length(notin1) > 1){
		      nb <- length(notin1)
		      seq <- paste(rep("N", dim(x1)[2]), collapse = "")
		      seq <- rep(seq, length = nb)
		      x22 <- x2[rownames(x2) %in% notin1, ]
		      nam <- rownames(x22)
		      ns <- list(nb = nb, seq = seq, nam = nam, com = NA)
		      class(ns) <- "alignment"
		      ns <- as.DNAbin(ns)
		      yy <- cbind(ns, x22)
		      y <- rbind(y, yy)
		    }
		    if (length(notin1) == 1){
		      nb <- 2
		      seq <- paste(rep("N", dim(x1)[2]), collapse = "")
		      seq <- rep(seq, length = nb)
		      x22 <-as.character(x2[rownames(x2) %in% notin1, ])				
		      x22 <- as.DNAbin(rbind(x22, x22))
		      rownames(x22) <- nam <- c(notin1, "DUMMY")
		      ns <- list(nb = nb, seq = seq, nam = nam, com = NA)
		      class(ns) <- "alignment"
		      ns <- as.DNAbin(ns)
		      yy <- cbind(ns, x22)
		      y <- rbind(y, yy)
		    }
		    # sequences that are not in 2
		    if (length(notin2) > 1){
		      nb <- length(notin2)
		      seq <- paste(rep("N", dim(x2)[2]), collapse = "")
		      seq <- rep(seq, length = nb)
		      x11 <- x1[rownames(x1) %in% notin2, ]
		      nam <- rownames(x11)
		      ns <- list(nb = nb, seq = seq, nam = nam, com = NA)
		      class(ns) <- "alignment"
		      ns <- as.DNAbin(ns)
		      yy <- cbind(x11, ns)
		      y <- rbind(y, yy)
		    }
		    if (length(notin2) == 1){
		      nb <- 2
		      seq <- paste(rep("N", dim(x2)[2]), collapse = "")
		      seq <- rep(seq, length = nb)
		      x11 <- as.character(x1[rownames(x1) %in% notin2, ])
		      x11 <- as.DNAbin(rbind(x11, x11))
		      rownames(x11) <- nam <- c(notin2, "DUMMY")
		      ns <- list(nb = nb, seq = seq, nam = nam, com = NA)
		      class(ns) <- "alignment"
		      ns <- as.DNAbin(ns)
		      yy <- cbind(x11, ns)
		      y <- rbind(y, yy)
		    }
		  }
		  dummy <- which(rownames(y) == "DUMMY")
		  if (length(dummy) > 0)
		    y <- y[-dummy, ]
		}
    return(y)
	}

	if (missing(single.list)){
	  x <- list(...)
	}
  else {
    x <- single.list
  }
	
	x1 <- x[[1]]
	x2 <- x[[2]]
	y <- c.two.genes(x1, x2)
	
	if (length(x) > 2){ 
	  for (i in 3:(length(x))) {								
	    x1 <- y
	    x2 <- x[[i]]
	    y <- c.two.genes(x1, x2)
	  }
	}
	y
}