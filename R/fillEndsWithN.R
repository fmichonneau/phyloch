`fillEndsWithN` <- 
function(x, sym = "n"){
	
	x <- as.alignment(x)
	
	turn <- function(x){
		x <- paste(rev(unlist(strsplit(x, ""))), collapse = "")
		x
	}
	
	fillin.n <- function(s){
		ss <- gsub("^-+", sym, s)
		diff <- nchar(s) - nchar(ss)
		if (diff > 0){
			ns <- paste(rep(sym, diff), collapse = "")
			s <- paste(ns, ss, sep = "")
		}												else s <- ss
		
		
		s
	}
	
	for (i in 1:x$nb){
		s <- x$seq[i]
		s <- fillin.n(s)
		s <- turn(s)
		s <- fillin.n(s)
		s <- turn(s)
		x$seq[i] <- s
 	}	
		
	x <- as.DNAbin(x)
	x	
}