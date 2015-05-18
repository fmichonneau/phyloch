delete.empty.cells <- function(alignment, 
                               nset = c("-", "n", "N", "?"), 
                               quiet = FALSE){
	
	s <- as.character(alignment)
	
	is.empty <- function(x, nset){
		ifelse(all(unique(x) %in% nset), FALSE, TRUE)
	}
	
	# columns
	colind <- which(apply(s, 2, is.empty, nset = nset))
	alignment <- alignment[, colind]
	ncol <- ncol(s) - length(colind)
  if ( !quiet ) cat(paste("\n", ncol, " columns deleted from alignment", sep = ""))
	
	# rows
	rowind <- which(apply(s, 1, is.empty, nset = nset))
	alignment <- alignment[rowind, ]
	nrow <- nrow(s) - length(rowind)
	if ( !quiet ) cat(paste("\n", nrow, " rows deleted from alignment", sep = ""))
	
	alignment
}

