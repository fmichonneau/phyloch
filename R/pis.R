pis  <- function(x, abs = TRUE, use.ambiguities = FALSE){

	pars.inf <- function(x){
		x <- table(x)
		x <- x[x > 1] # drop apomorphic chars
		n <- c("-", "n", "b", "h", "d", "v", "k", "s", "r", 			"w", "y")
		if (length(x[!names(x) %in% n]) > 1)				x  <-  TRUE									else FALSE
	}
	nbchar <- dim(x)[2]
	x  <-  as.character(x)
	out  <-  apply(x, 2, pars.inf)
	out <- length(out[out])
	if (!abs) out <- round(out / nbchar * 100, digits = 2)
	out
}