mrbayes <- 
function(x, file = "", nst = 6, rates = "invgamma", ngammacat = 4, nruns = 2, ngen = 1000000, printfreq = 100, samplefreq = 10,  nchains = 4, savebrlens = "yes", temp = 0.2, burnin = 10, contype = "allcompat", run = FALSE){
	
	# create MrBayes block
	# --------------------
	bayes <- c(
	    "\nbegin mrbayes;",
	    paste("\tlset nst=", nst, " rates=", 		
	        rates, " ngammacat=", ngammacat, ";", sep = ""),
        paste("\tmcmc nruns=", nruns, " ngen=", 		
	        as.integer(ngen), " printfreq=", printfreq, 		
	        " samplefreq=", samplefreq, " nchains=", 		
	        nchains, " savebrlens=", savebrlens, 			
	        " temp=", temp, ";", sep = ""),
	    paste("\tsumt filename=", file, " burnin=", 		
	        burnin, " contype=", contype, ";", sep = ""),
	    "end;"
	)
	
	# assemble and print NEXUS file
	# -----------------------------
	nexus <- write.nex(x, interleave = FALSE)
	nexus <- c(nexus, bayes)
	if (file == "") return(nexus)
	else write(nexus, file = file)
	
	# start mrbayes
	# -------------
	if (run){
		if (.Platform$OS.type == "unix")
			system(paste("mb > execute", file))
		else 												
		    system(paste("mrbayes ", file, ".bayes", sep = ""))

		tr <- read.nexus(paste(file, ".con.tre", sep = ""))
		return(tr)
	}
}
