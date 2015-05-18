mafft <- function(x, y, add, method = "localpair", maxiterate = 1000, 
                  op = 1.53, ep = 0.0, path, quiet){
  
  ## CHECKS and DEFINITIONS
  ## ----------------------
  os <- .Platform$OS
  if ( missing(quiet) ) quiet <- TRUE
  #qut <- ifelse(quiet, " --quiet ", " ")
  qut <- " "
  if ( missing(path) ) path <- "/usr/local/bin/mafft"
  fns <- c("mafftin.fas", "mafftpro.fas", "mafftout.fas")
  unlink(fns[file.exists(fns)])
  
  ## write input files and prepare call to MAFFT 
  ## -------------------------------------------
  if ( missing(y) ){
    write.fas(x, fns[1])
    call.mafft <- paste(path, " --", method, " --",   	
                        "maxiterate ", maxiterate, qut, "--op ", op, 		
                        " --ep ", ep, " ", fns[1], " > ", fns[3], sep = "")
  }
	else {
    if ( missing(add) ) add <- "addprofile"
    add <- match.arg(add, c("add", "addprofile"))
    add <- paste("--", add, sep = "")
	  write.fas(x, fns[1])
	  write.fas(y, fns[2])
	  call.mafft <- paste(path, qut, add, fns[2], fns[1], ">", fns[3])
	}
  if ( !quiet ) message(call.mafft)
  
  ## execute MAFFT on UNIX
  ## ---------------------
  if ( os == "unix" ){
    system(call.mafft, intern = FALSE)
    res <- length(scan(fns[3], what = "c", quiet = TRUE))
    if ( res != 0 ) res <- read.fas(fns[3])
  }
  ## execute MAFFT on WINDOWS
  ## ------------------------
  else {
    res <- system(call.mafft, intern = TRUE, ignore.stderr = FALSE)
    if ( length(grep("error|ERROR", res)) > 0 ){
      res <- 0
    }
    else {
      res <- read.fas(fns[3])
    }
  }

  unlink(fns[file.exists(fns)])
  return(res)
}

