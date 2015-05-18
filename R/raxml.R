raxml <- function(x, runs = 10, partition = NULL, outgroup = NULL, 
                  backbone = NULL, optimize = FALSE, clear = TRUE, 
                  file = "fromR", path = "/Applications/RAxML-7.0.4"){
				
	# definitions:
	# ------------
	rwd <- getwd()
	setwd(path)
  
	## do non-parametric bootstrapping?
  ## --------------------------------
	bs <- length(runs) == 2
	
	## check spelling of outgroup taxa
  ## -------------------------------
	if ( bs ){
		outgroupcheck <- outgroup %in% rownames(x)
		if ( !all(outgroupcheck) )
			stop(paste("The following outgroup taxa are", 				
                 "mispelled:\n", outgroup[!outgroupcheck]))
	}
	
	##  write phylip file
  ## ------------------
  
	write.phy(x, file)
	
	# write.partition file
	if ( !is.null(partition) ) {
		q <- paste(partition[,1], ", ",  
               partition[,2], " = ", 			
               partition[,3], sep = "")
		write(q, "partitionsFromR")
		q <- " -q partitionsFromR "
	} else q <- " "
		
	
	# clear previous runs
	# -------------------
	unlink(list.files(pattern = "RAxML_"))
	
	
	##########################################################
	# 	test initial rearrangement
	##########################################################
	
	if ( optimize ){
		system(paste("./raxmlHPC -y -s ", file ,
			" -m GTRCAT -n ST0", sep=""))
		system(paste("./raxmlHPC -y -s ", file ,
			" -m GTRCAT -n ST1", sep=""), 				show.output.on.console = FALSE)
		system(paste("./raxmlHPC -y -s ", file ,			" -m GTRCAT -n ST2", sep=""), 				show.output.on.console = FALSE)
		system(paste("./raxmlHPC -y -s ", file ,			" -m GTRCAT -n ST3", sep=""), 				show.output.on.console = FALSE)
		system(paste("./raxmlHPC -y -s ", file ,			" -m GTRCAT -n ST4", sep=""), 				show.output.on.console = FALSE)
		system(paste("./raxmlHPC -f d -i 10 -m GTRCAT -s ", 			file ," -t RAxML_parsimonyTree.ST0 -n FI0", 			sep=""), show.output.on.console = FALSE)
		system(paste("./raxmlHPC -f d -i 10 -m GTRCAT -s ", 			file ," -t RAxML_parsimonyTree.ST1 -n FI1", 			sep=""), show.output.on.console = FALSE)
		system(paste("./raxmlHPC -f d -i 10 -m GTRCAT -s ", 			file ," -t RAxML_parsimonyTree.ST2 -n FI2", 			sep=""), show.output.on.console = FALSE)
		system(paste("./raxmlHPC -f d -i 10 -m GTRCAT -s ", 			file ," -t RAxML_parsimonyTree.ST3 -n FI3", 			sep=""), show.output.on.console = FALSE)
		system(paste("./raxmlHPC -f d -i 10 -m GTRCAT -s ", 			file ," -t RAxML_parsimonyTree.ST4 -n FI4", 			sep=""), show.output.on.console = FALSE)
		system(paste("./raxmlHPC -f d -m GTRCAT -s ", file ,			" -t RAxML_parsimonyTree.ST0 -n AI0", sep=""), 			show.output.on.console = FALSE)
		system(paste("./raxmlHPC -f d -m GTRCAT -s ", file ,			" -t RAxML_parsimonyTree.ST0 -n AI1", sep=""), 			show.output.on.console = FALSE)
		system(paste("./raxmlHPC -f d -m GTRCAT -s ", file ,			" -t RAxML_parsimonyTree.ST0 -n AI2", sep=""), 			show.output.on.console = FALSE)
		system(paste("./raxmlHPC -f d -m GTRCAT -s ", file ,			" -t RAxML_parsimonyTree.ST0 -n AI3", sep=""), 			show.output.on.console = FALSE)
		system(paste("./raxmlHPC -f d -m GTRCAT -s ", file ,			" -t RAxML_parsimonyTree.ST0 -n AI4", sep=""), 			show.output.on.console = FALSE)
		x <- scan("RAxML_info.FI0", what="c", sep="", 			quiet = TRUE)
		FI0 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.FI1", what="c", sep="", 			quiet = TRUE)
		FI1 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.FI2", what="c", sep="", 			quiet = TRUE)
		FI2 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.FI3", what="c", sep="", 			quiet = TRUE)
		FI3 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.FI4", what="c", sep="", 			quiet = TRUE)
		FI4 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.AI0", what="c", sep="", 			quiet = TRUE)
		AI0 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.AI1", what="c", sep="", 			quiet = TRUE)
		AI1 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.AI2", what="c", sep="", 			quiet = TRUE)
		AI2 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.AI3", what="c", sep="", 			quiet = TRUE)
		AI3 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.AI4", what="c", sep="", 			quiet = TRUE)
		AI4 <- x[grep("Final", x)-1]
		FIXED <- mean(as.numeric(c(FI0, FI1, FI2, FI3, FI4)))
		AUTO <- mean(as.numeric(c(AI0, AI1, AI2, AI3, AI4)))
		# set i
		if (AUTO > FIXED) {
			i <- x[grep("setting", x) + 1]
			i <- as.numeric(gsub(",", "", i))
		}
	
	
	##########################################################
	# 	number of categories
	##########################################################
		x <- paste("./raxmlHPC -f d -i ", i, " -m GTRCAT -s ", 			file ," -t RAxML_parsimonyTree.ST", sep="")
		system(paste(x, "0 -c 10 -n C10_0", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "1 -c 10 -n C10_1", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "2 -c 10 -n C10_2", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "3 -c 10 -n C10_3", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "4 -c 10 -n C10_4", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "0 -c 40 -n C40_0", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "1 -c 40 -n C40_1", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "2 -c 40 -n C40_2", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "3 -c 40 -n C40_3", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "4 -c 40 -n C40_4", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "0 -c 55 -n C55_0", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "1 -c 55 -n C55_1", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "2 -c 55 -n C55_2", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "3 -c 55 -n C55_3", sep=""), 			show.output.on.console = FALSE)
		system(paste(x, "4 -c 55 -n C55_4", sep=""), 			show.output.on.console = FALSE)
		x <- scan("RAxML_info.C10_0", what="c", sep="", 
			quiet = TRUE)
		C10_0 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.C10_1", what="c", sep="", 
			quiet = TRUE)
		C10_1 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.C10_2", what="c", sep="", 
			quiet = TRUE)
		C10_2 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.C10_3", what="c", sep="", 
			quiet = TRUE)
		C10_3 <- x[grep("Final", x)-1]
		x <- scan("RAxML_info.C10_4", what="c", sep="", 
			quiet = TRUE)
		C10_4 <- x[grep("Final", x)-1]
		C10 <- mean(as.numeric(c(C10_0, C10_1, C10_2, C10_3, 			C10_4)))
	
	x <- scan("RAxML_info.C40_0", what="c", sep="", quiet = TRUE)
	C40_0 <- x[grep("Final", x)-1]
	x <- scan("RAxML_info.C40_1", what="c", sep="", quiet = TRUE)
	C40_1 <- x[grep("Final", x)-1]
	x <- scan("RAxML_info.C40_2", what="c", sep="", quiet = TRUE)
	C40_2 <- x[grep("Final", x)-1]
	x <- scan("RAxML_info.C40_3", what="c", sep="", quiet = TRUE)
	C40_3 <- x[grep("Final", x)-1]
	x <- scan("RAxML_info.C40_4", what="c", sep="", quiet = TRUE)
	C40_4 <- x[grep("Final", x)-1]
	C40 <- mean(as.numeric(c(C40_0, C40_1, C40_2, C40_3, C40_4)))
	
	x <- scan("RAxML_info.C55_0", what = "c", sep = "", 		quiet = TRUE)
	C55_0 <- x[grep("Final", x)-1]
	x <- scan("RAxML_info.C55_1", what = "c", sep = "", 		quiet = TRUE)
	C55_1 <- x[grep("Final", x)-1]
	x <- scan("RAxML_info.C55_2", what = "c", sep = "", 		quiet = TRUE)
	C55_2 <- x[grep("Final", x)-1]
	x <- scan("RAxML_info.C55_3", what = "c", sep = "", 		quiet = TRUE)
	C55_3 <- x[grep("Final", x)-1]
	x <- scan("RAxML_info.C55_4", what = "c", sep = "", 		quiet = TRUE)
	C55_4 <- x[grep("Final", x)-1]
	C55 <- mean(as.numeric(c(C55_0, C55_1, C55_2, C55_3, C55_4)))
	
	C <- if (FIXED > AUTO) c(C10, FIXED, C40, C55) 		else c(C10, AUTO, C40, C55)
	catnum <- c(10, 25, 40, 55)
	DF <- cbind(catnum, C)
	DF <- DF[order(DF[,2], decreasing = TRUE),]
	numcat <- DF[1,1]
	
	} # end of OPTIMIZE
	else {
		i <- 10
		numcat = 25
	}
		
	
	
	##########################################################
	# 	Find BEST TREE	
	##########################################################
	
	if ( !bs ){
		
		if ( !is.null(backbone) ){
			write.tree(backbone, "backbone.tre")
			g <- " -g backbone.tre"
		}
		else
			g <- " "
		# prepare calls
		if ( optimize ) call.phy <- paste("./raxmlHPC -f d -i ", i, 
			" -c ", numcat, " -m GTRCAT", q,"-s ", file , 			" -# ", runs, g," -n ", file, ".tre", sep = "")
		else call.phy <- paste("./raxmlHPC -f d -m GTRCAT", q, 			"-s ", file ," -# ", runs, g, " -n ", file, 			".tre", sep = "")
		
		# execute calls
		cat("\nFind best ML tree ...")
		system(call.phy)
		resname <- paste("RAxML_info.", file, ".tre", sep = "")
		res <- scan(resname, quiet = TRUE, what = "char", 			sep = "\n")
		if (length(grep("exiting", res)) > 0)
			stop("\n", paste(res, collapse = "\n"))
		cat(" done.\n")
		
		if (optimize){
			cat(paste("\nE(logLik) with fixed rearrangement", 				" settings:", FIXED))
			cat(paste("\nE(logLik) with automatic rearrangement", 				" settings:", AUTO))
		}
		cat("\nRAxML was called as follows:\n")
		# call raxml
		cat(call.phy)
		 
		# identify and read best tree
		if ( runs > 1 ){
			x <- scan(paste(path, "/RAxML_info.", file, ".tre", sep = ""), 
                what = "c", sep = "", quiet = TRUE)
			i <- as.integer(gsub(":", "", x[grep("Best", x) + 5]))
			best.tree.file <- paste("RAxML_result.", file, 				
                              ".tre.RUN.", i, sep = "")
		} else {
      best.tree.file <- paste("RAxML_result.", file, ".tre", sep = "")
		}
		BEST.TREE <- read.tree(best.tree.file)
    
    
		# root tree
		if ( !is.null(outgroup[1]) )
			BEST.TREE <- root(BEST.TREE, outgroup)
		setwd(rwd)
		TREENAME <- paste("tre.", file, sep = "")
		write.tree(BEST.TREE, file = TREENAME)
		cat("\n\nThe best tree obtained was printed as ", 
        TREENAME, " to your working directory", sep = "")
		if ( clear ){
			dir.create(paste("RAxML.", file, sep = ""))
			PATH <- paste(rwd, "/RAxML.", file, "/", sep = "")
			setwd(path)
			system(paste("mv ", file, " ", PATH, file, sep = ""))
			system(paste("mv RAxML_info.", file, ".tre ", PATH, 				"RAxML_info.", file, ".tre", sep = ""))
			if ( runs > 1 ){
				for ( i in 0:(runs-1) ){
				    system(paste("mv RAxML_log.", file, 					".tre.RUN.", i, " ", PATH, "RAxML_log.", 					file, ".tre.RUN.", i, sep = ""))
					system(paste("mv RAxML_parsimonyTree.", file, 					 	".tre.RUN.", i, " ", PATH, 						"RAxML_parsimonyTree.", file, 						".tre.RUN.", i, sep = ""))
					system(paste("mv RAxML_result.", file, 						".tre.RUN.", i, " ", PATH, 						"RAxML_result.", file, ".tre.RUN.", 					 	i, sep = ""))
				}
			}
			else {
				system(paste("mv RAxML_log.", file, ".tre ", 					PATH, "RAxML_log.", file, ".tre", sep = ""))
				system(paste("mv RAxML_parsimonyTree.", file, 					".tre ", PATH, "RAxML_parsimonyTree.", 					file, ".tre", sep = ""))
				system(paste("mv RAxML_result.", file, ".tre ", 					PATH, "RAxML_result.", file, ".tre", 					sep = ""))
			}
			cat("\n\nThe entire RAxML output has been stored in ", 				PATH, "\n\n", sep="")
		}
		else cat("\n\nA log file, parsimony starting trees, and ", 
			"the other resulting trees are stored in ", path, 			"\n\n", sep = "")
		tree <- BEST.TREE
	}
		
	##########################################################
	# NON-PARAMETRIC BOOTSTRAPPING
	##########################################################	
	else {
		
		# generate random number seed
		seed <- round(runif(1) * 1e06)
		
		## backbone tree option
    ## --------------------
		if ( !is.null(backbone) ){
			write.tree(backbone, "backbone.tre")
			g <- " -r backbone.tre"
		}												else
			g <- " "
			
		# partition
		if ( !is.null(partition) ){
			write.tree(backbone, "backbone.tre")
			g <- " -r backbone.tre"
		}												else
			g <- " "
		
		# assemble call for optimzed runs
		if ( optimize ){
			call.phy <- paste("./raxmlHPC -f d -i ", i, " -c ", 				
                        numcat, " -m GTRCAT -s ", file , " -# ", 				
                        runs[1], g, q, " -n ", file, ".tre", sep = "") 
			call.bs <- paste("./raxmlHPC -f i -i ", i, " -c ", 				
                       numcat, " -m GTRCAT -s ",file, " -# ", 				
                       runs[2], g, q, " -b ", seed, " -n ", file, 				
                       ".boot", sep = "")		
		}
		# assemble call for non-optized runs
		else {
			call.phy <- paste("./raxmlHPC -f d -m GTRCAT -s ",
                        file ," -# ", runs[1], g, " -n ", file, 				
                        ".tre", sep = "") # get best tree
			call.bs <- paste("./raxmlHPC -f i -i ", i, " -c ", 				
                       numcat, " -m GTRCAT -s ", file ," -# ", 				
                       runs[2], g, " -b ", seed, " -n ", file, ".boot",	
                       sep = "")
		}
		
		cat("\nFind best ML tree ...")
		system(call.phy)
		cat(" done.\n")
		cat("Non-parametric bootstrapping ...")
		system(call.bs)
		cat(" done.\n")
		
		
		# 1. Identify best tree
		x <- scan(paste(path, "/RAxML_info.", file, 			".tre", sep = ""), what = "c", sep = "", quiet = TRUE)
		i <- as.integer(gsub(":", "", x[grep("Best", x) + 5]))
		# 2. Read in best tree
		BEST.TREE <- read.tree(paste("RAxML_result.", file, 			".tre.RUN.", i, sep = ""))
		# 3. Root best tree 
		if ( !is.null(outgroup[1]) )
			BEST.TREE <- root(BEST.TREE, outgroup)
		# 4. schreibe ihn in RAxml: rootedbesttreefromR 
		write.tree(BEST.TREE, file = "rootedbesttreefromR")
		
		# 5. map biparitions on best tree 
		z <- paste("RAxML_bootstrap", file, "boot", sep = ".")
		# -o: outgroup flag
		o <- " "
		if ( !is.null(outgroup) ){
			out <- paste(outgroup, collapse = ",")
			o <- paste(" -o", out)
		}
		call.bip <- paste("./raxmlHPC -f b -m GTRCAT -s ", 
                      file, " -z ", z, o, " -t rootedbesttreefromR", 			" -n ", file, ".bipart", sep = "")
		system(call.bip)
		
		# 6. Read supported tree
		# ----------------------
		SUPPORT.TREE <- read.tree(paste("RAxML_bipartitions", 			file, "bipart", sep = "."))
	
		current <- unlist(strsplit(format(Sys.time()), " "))[1]
		PATH <- paste(rwd, "/RAxML.", file, current, "/", 			sep = "")
		TREENAME <- paste(PATH, "bootstrap.", file, sep = "")
		
		cat("\nRAxML was called as follows:")
		cat(paste("\n", call.phy, sep = ""))
		cat(paste("\n", call.bs, sep = ""))
		cat(paste("\n", call.bip, sep = ""))
		
		
		# 7. transfer files to R working directory
		# ----------------------------------------
		if ( clear ){
			dir.create(PATH)
			write.tree(SUPPORT.TREE, TREENAME)
			from <- list.files(pattern = "RAxML_")
			to <- paste(PATH, from, sep = "")
			file.copy(from, to)
			file.remove(from)
			system(paste("mv", file, PATH))
			system(paste("mv rootedbesttreefromR ", PATH, 				sep = ""))
			cat("\n\nThe entire RAxML output has been stored in ", 				PATH, "\n\n", sep = "")
		}
		else cat("\n\nA log file, parsimony starting trees, and ", 
			"the other resulting trees are stored in ", path, 			"\n\n", sep = "")
		tree <- SUPPORT.TREE
	}
	setwd(rwd)
	tree
}