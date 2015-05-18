delete.nonoverlapping.seq <- function(x){
	
	l <- vector("list", length = dim(x)[1])
	for (i in seq(x[, 1])){
		l[[i]] <- grep("a|c|g|t", as.character(x[i, ]))
	}
	out <- vector()
	for (i in 1:(dim(x)[1] - 1)){
		for (j in (i + 1):dim(x)[1]){
			is <- intersect(l[[i]], l[[j]])
			if(length(is) == 0)
			out <- c(out, i, j)
		}
	}
	out <- unique(out)
	dt <- rownames(x)[out]
	cat("Deleted from original alignment:\n\n",			paste(dt, collapse = "\n"), sep = "")
	x <- x[-out, ]
}