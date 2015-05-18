mrp <- 
function(... , method = "BR"){
	
	obj <- list(...)
	ntree <- length(obj)
    cat(paste("\nGetting matrix representation of", ntree, 		"trees."))
    
	L <- NULL
	for (k in 1:ntree){
		tree <- obj[[k]]
		# get node and tip numbers
		nodes <- tree$Nnode
		tips <- length(tree$tip.label)
		
		if (method == "BR"){ # BAUM & RAGAN coding scheme
			# create matrix with zeros
			M <- matrix(0, ncol = nodes, nrow = tips)
			rownames(M) <- tree$tip.label
			# get MATRIX REPRESENTATION iterativly
			for (i in unique(tree$edge[, 1])) {	# iteration over 				all internal nodes
				d <- descendants(tree, i)
				# fill in M matrix
				M[d, i-tips] <- 1
			}
		}
				
		if (method == "P") {# PURVIS coding scheme
			# create matrix with Ns
			M <- matrix("N", ncol = nodes, nrow = tips)
			rownames(M) <- tree$tip.label
			# get MATRIX REPRESENTATION iterativly
			for (i in unique(tree$edge[,1])) {	# iteration over 				all internal nodes
				d <- descendants(tree, i)
				M[d, i - tips] <- 1
				if (i > 1){
					s <- sister(tree, i)
					M[s, i - tips] <- 0
				}
			}
		}
			
		# concatenate matrices
		if (length(L) == 0) 								L <- M 										else 												L <- c.binary(L, M, match = FALSE)
	}	
	L
}