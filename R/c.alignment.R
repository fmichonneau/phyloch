c.alignment <- function(...){
	
	x <- list(...)
	
	if (class(x[[1]]) != "alignment")
		x[[1]] <- as.alignment(x[[1]])
	
	nb <- length(x)
	for (i in 2:nb)	{
		if (class(x[[i]]) != "alignment")
			x[[i]] <- as.alignment(x[[i]])
		x[[1]][[1]] <- x[[1]][[1]] + x[[i]][[1]]
		x[[1]][[2]] <- c(x[[1]][[2]], x[[i]][[2]])
		x[[1]][[3]] <- c(x[[1]][[3]], x[[i]][[3]])
	}
	x <- x[[1]]
}