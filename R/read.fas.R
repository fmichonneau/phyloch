read.fas <- function(x, text){	
	
  if ( !missing(text) )
    x <- text
  else
    x <- scan(x, what = character(), quiet 	= TRUE)
  if ( length(x) == 0 ) stop("file is empty")

  start <- grep("^ {0,}>", x)
  h <- unlist(strsplit(x[start][1], ""))
  if (length(h) == 1){
    taxnames <- x[start + 1]
    space <- 2
  }
  else {
    taxnames <- x[start]
    taxnames <- gsub(">", "", taxnames)
    space <- 1
  }
  ntax <- length(taxnames)
    
  start <- c(start, length(x) + 1)
  obj <- vector("list", ntax)
  
  for (i in 1:ntax) 
    obj[[i]] <- unlist(strsplit(gsub(" ", 
      "", x[(start[i] + space):(start[i + 1] - 1)]), NULL))
  names(obj) <- taxnames
    
  # test for equal sequence lengths
  # -------------------------------
  y <- vector(length = ntax)
  for (i in 1:ntax) y[[i]] <- length(obj[[i]])
  y <- length(unique(as.vector(unlist(y))))
  
  obj <- lapply(obj, tolower)
  obj <- ape::as.alignment(obj)
  if (y == 1)
    obj <- as.DNAbin(obj)
  obj
}
