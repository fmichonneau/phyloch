ident.seq <- 
function(x, quiet = TRUE){
	d <- as.matrix(dist.gene(x))
	d[upper.tri(d, diag = TRUE)] <- NA
	
	remove.seq <- vector()
	for (i in 1:dim(d)[2]){
		rs <- which(d[,i] == 0)
		remove.seq <- c(remove.seq, rs)
	}
	remove.seq <- unique(remove.seq)
	removed <- rownames(x)[remove.seq]
	x <- x[-remove.seq,]
	if (!quiet){
		cat(length(remove.seq), 							"identical sequence(s) removed from alignment:\n")
		for (i in seq(along = removed))
			cat(removed[i], "\n")
	}
	x
}