aliscore <- function(x, path){
	
	path <- "/Applications/Aliscore_v1.0"
	rwd <- getwd()
	setwd(path)
	write.fas(x, "input.fas")
	call <- "perl Aliscore.02.pl -i input.fas"
	system(call)
	id <- scan("input.fas_List-l_all.txt", what = "char", 
	    quiet = TRUE)
	id <- as.numeric(id)
	nid <- length(id)
	if (nid == 0)
	    cat("\nALISCORE did not remove any characters.")	else {
	    
	    x <- x[, -id]
	    cat("\nALISCORE removed", nid, "characters.")
	}
	setwd(rwd)
	x
}