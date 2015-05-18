identicalTips <- 
function(x, y, quiet = TRUE){
	getTips <- function(obj){
		if (class(obj) == "phylo")
			obj <- obj$tip.label
		if (class(obj) == "DNAbin")
			obj <- rownames(obj)
		if (class(obj) == "matrix")
			obj <- rownames(obj)
		obj
		}	
	x <- getTips(x)
	y <- getTips(y)	
	out <- all(all(x %in% y), all(y %in% x))	
	
	if (!out & !quiet){
		if (!all(x %in% y))
			cat("\nThe following tips are lacking from 'y':\n",
				paste(x[!x %in% y], collapse = "\n"), "\n\n")
		if (!all(y %in% x))
			cat("\nThe following tips are lacking from 'x':\n",
				paste(y[!y %in% x], collapse = "\n"), "\n\n")
	}
	out
}