homogen.base.freq <- function(x, tabulate.only = FALSE){

	x <- as.character(x)

	# delete missing and ambigues sites
	# ---------------------------------
	foo <- function(x){
		x <- unique(x)
		if (all(x %in% c("a", "t", "c", "g", "-")))
			x <- TRUE else FALSE
	}	
	ind <- apply(x, 2, foo)
	x <- x[, ind]
	
	# tabulate base frequencies for each taxon
	# ----------------------------------------
	foo <- function(x){
		x <- table(x)
	}	
	x <- apply(x, 1, foo)
	
	# if all taxa contain gap charactes the tabulation will
	# result in a matrix:
	# --------------------------------------
	if (is.matrix(x)){
	
		# delete gap counts from table
		# ----------------------------------------
		hyphen <- "-" %in% rownames(x)
		if (hyphen)
		 	x <- x[-which(hyphen), ]
			
		# turn list into a matrix
		# -------------------------
		x <- t(x)	
	}

	# list will result if at least one taxon has no gap 
	# characters (-)
	# ------------------------------
	if (is.list(x)){
	
		# delete gap counts from table
		# ----------------------------------------
		foo <- function(x){
			hyphen <- "-" %in% names(x)
			if (hyphen)
		 		x <- x[-which(hyphen)]
			 else x <- x
		}	
		x <- lapply(x, foo)
	
		# turn list into a matrix
		# -------------------------
		x <- matrix(unlist(x), ncol = 4, byrow = TRUE)
	}
	
	# perform Chi-square test
	# -----------------------
	if (!tabulate.only)
		x <- chisq.test(x)
	x
}	
	
	