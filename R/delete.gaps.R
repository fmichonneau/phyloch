delete.gaps <- function(x){
	
	ncha <- dim(x)[2]
	hh <- vector()
	for (i in 1:ncha){
		if ("-" %in% as.character(x[,i]))
		hh <- c(hh, i)
	}
	s <- s[, -hh]
	s
}