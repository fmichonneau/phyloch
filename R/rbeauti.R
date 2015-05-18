rbeauti  <- function(path = "aaa.xml", s, taxonset = NULL, monophyly = NULL, tmrcaCons = NULL, startingTree = "random", specModel = "yule", clock = "strict", ngen = "10000000", samplefreq = "10000", logSubs = FALSE, nodata = FALSE){
	
	# manage filenames
	filename <- gsub("/.+/|.xml", "", path)
	logFileName <- paste(filename, "log", sep = ".")
	treeFileName <- paste(filename, "trees", sep = ".")
	subsTreeFileName <- paste(filename, "subst.trees", sep = ".")
	
	
	# rates model?
	clock <- match.arg(clock, c("strict", "lognormal", 		"exponential"))
	
	if (nodata){
		nd <- matrix("?", ncol= 1, nrow = dim(s)[1])
		rownames(nd) <- rownames(s)
		s <- as.DNAbin(nd)
	}
	
	# alignment as list amenable to getSeqList
	ss  <-  as.alignment(s)
	sss  <- as.list(toupper(ss$seq))
	sss  <- gsub("N", "?", sss)
	names(sss) <- ss$nam
	
	# number of taxa gives brachRate categories
	nbtax <- ss$nb
	brRateCat <- (nbtax * 2) - 2
	
	
	# input elements
	# --------------
	nb.taxonset  <- length(taxonset)
	nb.tmrca  <- length(tmrcaCons)
	nb.mono <- length(monophyly)
	true.monophyly  <- monophyly[monophyly == "true"]
	nb.true.mono  <- length(true.monophyly)
	
	getTaxonList <- function(x){
		x  <- xmlNode("taxon", attrs = c(id = x))
		x
	}
	
	getTaxonSetList <- function(x){
		x  <- xmlNode("taxon", attrs = c(idref = x))
		x
	}
	
	getSeqList <- function(x){
		x  <- xmlNode("sequence", 
			xmlNode("taxon", attrs = c(idref = names(x)), x)
		)
		x
	}	
	
	# create <taxa> node
	# -------------------------------
	taxa  <-  xmlNode("taxa", attrs = c(id = "taxa"))
	kids  <- lapply(rownames(s), getTaxonList)
	taxa  <-  addChildren(taxa, kids = kids)
	
	# create further <taxa> nodes
	# -------------------------------
	if (!is.null(taxonset)){
		treeModelREF <- xmlNode("treeModel", 				attrs = c(idref = "treeModel"))
		yy  <- xx  <- vector(mode = "list", 				length = length(taxonset))
		for (i in 1:length(taxonset)){
			taxname <- names(taxonset)[i]
			
			x <- xmlNode("taxa", 								attrs = c(id = taxname))
			kids  <- lapply(taxonset[[i]], getTaxonSetList)
			xx[[i]]  <-  addChildren(x, kids = kids)
		
			mn <- paste("tmrca(", taxname, ")", sep = "")
			yy[[i]]  <- xmlNode("tmrcaStatistic",
				attrs = c(id = mn),
				xmlNode("mrca",
					xmlNode("taxa", attrs = c(idref = taxname))),
						treeModelREF)
			
		}
	taxset  <- xx	
	}
	
	if (length(true.monophyly) > 0){
		treeModelREF <- xmlNode("treeModel", 				attrs = c(idref = "treeModel"))
		monostat  <- vector(mode = "list", length = nb.true.mono)
		for (i in 1:nb.true.mono){
			monostat[[i]]  <- xmlNode("monophylyStatistic",
			attrs = c(id = paste("monophyly(", 					names(true.monophyly)[i], ")", sep = "")),
			xmlNode("mrca",
				xmlNode("taxa", 									attrs = c(idref = names(true.monophyly)[i]))),
			treeModelREF)
		}
	}
	else monostat <- NULL
	
	
	
	# <alignment> node
	# -----------------------
	alignment  <-  xmlNode("alignment", 				attrs = c(id = "alignment", dataType = "nucleotide"))
	kids  <- vector(mode = "list", length = length(sss))
	for (i in 1:length(sss)){
		kids[[i]]  <- xmlNode("sequence", 
			xmlNode("taxon", attrs = c(idref = names(sss)[i])), 			sss[[i]])
		}
	alignment  <-  addChildren(alignment, kids = kids)
	
	
	# <patterns> node
	# -----------------------
	patterns <- xmlNode("patterns",
		attrs = c(id = "total.patterns", from = "1"),
		xmlNode("alignment",
			attrs = c(idref = "alignment")))
	# create idref node
	patternsREF <- xmlNode("patterns", 					attrs = c(idref = "total.patterns"))
	
	
	# ultrametric Speciation Model: ums
	
	# <yuleModel> element
	# -----------------------
	if (specModel == "yule"){
		ultrametricSpeciationModel <- xmlNode("yuleModel",
			attrs = c(id = "yule", units = "substitutions"),
			xmlNode("birthRate",
				xmlNode("parameter", 
					attrs = c(id = "yule.birthRate",
						value = "1.0",
						lower = "0.0",
						upper = "1000000.0"))))
		ultrametricSpeciationModelREF <- xmlNode("yuleModel", 			attrs = c(idref = "yule"))
		evoOperMCMCREF <- list(xmlNode ("parameter", 			attrs = c(idref = "yule.birthRate")))
	}
	
	# <birthDeathModel> element
	# -----------------------
	if (specModel == "birthDeath"){
		ultrametricSpeciationModel <- xmlNode("birthDeathModel",
			attrs = c(id = "birthDeath", units = "substitutions"),
			xmlNode("birthMinusDeathRate",
				xmlNode("parameter", 
					attrs = c(id = "birthDeath.meanGrowthRate",
						value = "1.0",
						lower = "0.0",
						upper = "Infinity")
				)
			),
			xmlNode("relativeDeathRate",
				xmlNode("parameter", 
					attrs = c(id = "birthDeath.relativeDeathRate",
						value = "0.5",
						lower = "0.0",
						upper = "1.0")
				)
			)	
		)
		ultrametricSpeciationModelREF <- 					xmlNode("birthDeathModel", 							attrs = c(idref = "birthDeath"))
		birthDeath.bMinusDRateREF <- 						xmlNode("birthDeath.bMinusDRate", 					attrs = c(idref = "birthDeath.bMinusDRate"))
		birthDeath.relativeDeathRateREF <- 					xmlNode("birthDeath.relativeDeathRate", 				attrs = c(idref = "birthDeath.relativeDeathRate"))
		evoOperMCMCREF <- list(
			xmlNode ("parameter", 			
				attrs = c(idref = "birthDeath.meanGrowthRate")), 
			xmlNode ("parameter", 
				attrs = c(idref = "birthDeath.relativeDeathRate"))
		)
	}
	
	# <constantSize> node
	# -----------------------
	if (specModel %in% c("yule", "birthDeath")){
		constantSize  <- xmlNode("constantSize",
			attrs = c(id = "initialDemo", 						units = "substitutions"),
			xmlNode("populationSize",
				xmlNode("parameter", 								attrs = c(id = "initialDemo.popSize",
						value = "100.0"))))
					
		# create IDREF node:
		constantREF <- xmlNode ("constantSize", 			attrs = c(idref = "initialDemo"))
		
	}												else {
		constantSize  <- xmlNode("constantSize",
			attrs = c(id = "constant", units = "substitutions"),
			xmlNode("populationSize",
				xmlNode("parameter", 								attrs = c(id = "constant.popSize",
						value = "1.0",
						lower = "0.0",
						upper = "100.0"))))
					
		# create IDREF node:
		constantREF <- xmlNode ("constantSize", 			attrs = c(idref = "constant"))
		evoOperMCMCREF <- xmlNode ("parameter", 			attrs = c(idref = "constant.popSize"))
	}
	
	
	# STARTING TREE
	# ----------------------------------------------------
	# USER-SPECIFIED starting tree: <newick> node
	if (class(startingTree) == "phylo"){
		startTree <- xmlNode("newick",
			attrs = c(id = "startingTree"),
			write.tree(startingTree))
		# create IDREF node:
		startTreeREF <- xmlNode("newick",
			attrs = c(idref = "startingTree"))
	}
	# RANDOM starting tree: <coalescentTree> node
	if (startingTree[[1]][1] == "random") {
		constrainedTaxa <- xmlNode("constrainedTaxa")
		tax <- xmlNode("taxa", attrs = c(idref = "taxa"))
		constrainedTaxa <- addChildren(constrainedTaxa, 			kids = list(tax))

		if (!is.null(monophyly)){
			x <- vector(mode = "list", length = length(monophyly))
			for (i in 1:length(monophyly))
				x[[i]] <- xmlNode("tmrca", 
					attrs = c(monophyletic = 							as.vector(monophyly[i])),					xmlNode("taxa", 
						attrs = c(idref = names(monophyly)[i])))
			
			constrainedTaxa  <- addChildren(constrainedTaxa, 				kids = x)
		}

		startTree  <- xmlNode("coalescentTree",
			attrs = c(id = "startingTree"))
		
		startTree  <- addChildren(startTree, 				kids = list(constrainedTaxa, constantREF))
		# create IDREF node:
		startTreeREF <- xmlNode("coalescentTree",
			attrs = c(idref = "startingTree"))
	}
	# UPGMA starting tree: <upgmaTree> node
	if (startingTree[[1]][1] == "upgma"){
		distanceMatrix <- xmlNode("distanceMatrix",
			attrs = c(correction = "JC"))
		distanceMatrix  <- addChildren(distanceMatrix, 				kids = list(xmlNode("patterns",
				.children = list(
				xmlNode("alignment", 
				attrs = c(idref = "alignment"))))))
		startTree  <- xmlNode("upgmaTree",
			attrs = c(id = "startingTree"))
		startTree  <- addChildren(startTree, 				kids = list(distanceMatrix))	
		
		startTreeREF <- xmlNode("upgmaTree",
			attrs = c(idref = "startingTree"))
	}
	
	
		
	# <treeModel> node
	# ------------------------
	treeModel  <- xmlNode("treeModel",
		attrs = c(id = "treeModel"))
		
	# create children		
	rootHeight <- xmlNode("rootHeight",
		xmlNode("parameter", 
			attrs = c(id = "treeModel.rootHeight")))
			
	nodeHeights1 <- xmlNode("nodeHeights",
		attrs = c(internalNodes = "true"),
		xmlNode("parameter",
			attrs = c(id = "treeModel.internalNodeHeights")))
			
	nodeHeights2 <- xmlNode("nodeHeights",
		attrs = c(internalNodes = "true", rootNode = "true"),
		xmlNode("parameter",
			attrs = c(id = "treeModel.allInternalNodeHeights")))

	# add children
	treeModel  <- addChildren(treeModel,
		kids = list(startTreeREF,
			rootHeight,
			nodeHeights1,
			nodeHeights2))
			
	# create reference nodes:
	treeModelREF <- xmlNode("treeModel", 				attrs = c(idref = "treeModel"))
	treeModel.rootHeightREF <- xmlNode("parameter", 		attrs = c(idref = "treeModel.rootHeight"))
	treeModel.internalNodeHeightsREF <- xmlNode("parameter", 		attrs = c(idref = "treeModel.internalNodeHeights"))
	treeModel.allInternalNodeHeightsREF <- xmlNode("parameter", 		attrs = c(idref = "treeModel.allInternalNodeHeights"))

	
	# <speciactionLikelihood> AND <coalescentLikelihood> node
	# -------------------------------------------------------
	if (specModel %in% c("yule", "birthDeath")){
		evoLikelihood  <- xmlNode("speciationLikelihood",
			attrs = c(id = "speciation"),
			xmlNode("model", ultrametricSpeciationModelREF),
			xmlNode("speciesTree", treeModelREF))
	
		# create IDREF node:
		evoLikelihoodREF <- xmlNode("speciationLikelihood", 			attrs = c(idref = "speciation"))

	}												else {
		evoLikelihood  <- xmlNode("coalescentLikelihood",
			attrs = c(id = "coalescent"),
			xmlNode("model", constantREF),
			xmlNode("populationTree", treeModelREF))
	
		# create IDREF node:
		evoLikelihoodREF <- xmlNode("coalescentLikelihood", 			attrs = c(idref = "coalescent"))
	}
	
	
	
	# CLOCK MODEL
	# ------------------------------
	if (clock == "strict"){
		branchRates <- xmlNode("strictClockBranchRates",
			attrs = c(id = "branchRates"),
				xmlNode("rate",
					xmlNode("parameter", 
						attrs = c(id = "clock.rate",
							value = "0.0042",
							lower = "0.0",
							upper = "100.0"))))
							
		# create IDREF node
		branchRatesREF <- xmlNode("strictClockBranchRates",
				attrs = c(idref = "branchRates"))
		clock.rateREF <- xmlNode("parameter",
				attrs = c(idref = "clock.rate"))
	}
	if (clock == "lognormal"){
		
		branchRates  <- xmlNode	("discretizedBranchRates",
			attrs = c(id = "branchRates"))
			
		mean <- xmlNode("mean", 							xmlNode("parameter", 								attrs = c(id = "ucld.mean",
					value = "0.0028",
					lower = "0.0",
					upper = "100.0")))
			
		stdev <- xmlNode("stdev",
			xmlNode("parameter", 								attrs = c(id = "ucld.stdev",
					value = "0.1",
					lower = "0.0",
					upper = "10.0")))
					
		logNormalDistributionModel  <- xmlNode				("logNormalDistributionModel",
				attrs = c(meanInRealSpace = "true"))
		logNormalDistributionModel  <- addChildren			(logNormalDistributionModel, 						kids = list(mean, stdev))
		distribution <- xmlNode("distribution")
		distribution <- addChildren(distribution, 			kids = list(logNormalDistributionModel))


	
		rateCategories <- xmlNode("rateCategories",
			xmlNode("parameter", 								attrs = c(id = "branchRates.categories", 				dimension = brRateCat)))
			
		branchRates <- addChildren(branchRates,
			kids = list(treeModelREF, distribution,
				rateCategories))
				
		# create IDREF node
		branchRatesREF <- xmlNode("discretizedBranchRates",
				attrs = c(idref = "branchRates"))
		branchRatesCategoriesREF <- 						xmlNode("parameter",
				attrs = c(idref = "branchRates.categories"))
		ucld.meanREF <- xmlNode("parameter",
				attrs = c(idref = "ucld.mean"))
		ucld.stdevREF <- xmlNode("parameter",
				attrs = c(idref = "ucld.stdev"))
				
	# <rateStatistic> node: meanrate
	# ------------------------------
	
	ratestat1 <- xmlNode("rateStatistic", 
		attrs = c(id = "meanRate",
			name = "meanRate",
			mode = "mean",
			internal = "true",
			external = "true"),
		treeModelREF,
		branchRatesREF
	)
	
	# <rateStatistic> node: coefficientOfVariation
	# --------------------------------------------
	ratestat2 <- xmlNode("rateStatistic", 
		attrs = c(id = "coefficientOfVariation",
			name = "coefficientOfVariation",
			mode = "coefficientOfVariation",
			internal = "true",
			external = "true"),
		treeModelREF,
		branchRatesREF)
	
	# create IDREF node:
	rateStatisticREF <- xmlNode("rateStatistic", 		attrs = c(idref = "coefficientOfVariation"))

	
	# <rateCovarianceStatistic> node
	# ------------------------------
	rateCovStat <- xmlNode("rateCovarianceStatistic", 
		attrs = c(id = "covariance",
			name = "covariance"),
		treeModelREF,
		branchRatesREF)
		
	# create IDREF node:
	rateCovarianceStatisticREF <- 						xmlNode("rateCovarianceStatistic", 					attrs = c(idref = "covariance")) 

		
	} # end of clock == "lognormal"
	
	# <siteModel> node:
	# ------------------------------
	
	gtrModel  <- xmlNode("gtrModel", 					attrs = c(id = "gtr"),
			xmlNode("frequencies", 								xmlNode("frequencyModel", 							attrs = c(dataType = "nucleotide"),
					xmlNode("frequencies", 								xmlNode("parameter", 							   attrs = c(id = "frequencies", 							  value = "0.25 0.25 0.25 0.25"))))),
					xmlNode("rateAC", 									xmlNode("parameter", 							attrs = c(id = "ac", 		value = "1.0", lower = "1.0E-8", upper = "100.0"))),
	xmlNode("rateAG", xmlNode("parameter", attrs = c(id = "ag", 		value = "1.0", lower = "1.0E-8", upper = "100.0"))),
	xmlNode("rateAT", xmlNode("parameter", attrs = c(id = "at", 		value = "1.0", lower = "1.0E-8", upper = "100.0"))),
	xmlNode("rateCG", xmlNode("parameter", attrs = c(id = "cg", 		value = "1.0", lower = "1.0E-8", upper = "100.0"))),
	xmlNode("rateGT", xmlNode("parameter", attrs = c(id = "gt", 		value = "1.0", lower = "1.0E-8", upper = "100.0")))
)
	# create IDREF nodes:
	acREF  <- xmlNode("parameter", 						attrs = c(idref = "ac"))
	agREF  <- xmlNode("parameter", 						attrs = c(idref = "ag"))
	atREF  <- xmlNode("parameter", 						attrs = c(idref = "at"))
	cgREF  <- xmlNode("parameter", 						attrs = c(idref = "cg"))
	gtREF  <- xmlNode("parameter", 						attrs = c(idref = "gt"))
	frequenciesREF <- xmlNode("parameter", 				attrs = c(idref = "frequencies"))
	
	# <siteModel> node:
	# ------------------------------
	siteModel  <-  xmlNode("siteModel",
		attrs = c(id = "siteModel"),
		xmlNode("substitutionModel",
			xmlNode("gtrModel",
				attrs = c(idref = "gtr"))),
		xmlNode("gammaShape",
			attrs = c(gammaCategories = "8"),
			xmlNode("parameter", 
				attrs = c(id = "alpha",
					value = "0.5",
					lower = "0.0",
					upper = "100.0"))))
	# create IDREF node:
	siteModel.alphaREF <- xmlNode("parameter", 			attrs = c(idref = "alpha"))
	
	# <treeLikelihood> node:
	# ------------------------------
	treeLikelihood  <- xmlNode("treeLikelihood",
		attrs = c(id = "total.treeLikelihood", 
			useAmbiguities = "false"),
		patternsREF,
		treeModelREF,
		xmlNode("siteModel", attrs = c(idref = "siteModel")),
		branchRatesREF)
		
	# create IDREF node:
	treeLikelihoodREF  <-  xmlNode("treeLikelihood", 		attrs = c(idref = "total.treeLikelihood"))
	
	
	# <operators> node
	# ----------------
	operators  <- xmlNode("operators",
	attrs = c(id = "operators"))
	
	kids1  <- list(
		xmlNode("scaleOperator", 		
			attrs = c(scaleFactor = "0.75", weight = "1"),
			acREF),
		xmlNode("scaleOperator", 		
			attrs = c(scaleFactor = "0.75", weight = "1"),
			agREF),
		xmlNode("scaleOperator", 		
			attrs = c(scaleFactor = "0.75", weight = "1"),
			atREF),
		xmlNode("scaleOperator", 		
			attrs = c(scaleFactor = "0.75", weight = "1"),
			cgREF),
		xmlNode("scaleOperator", 		
			attrs = c(scaleFactor = "0.75", weight = "1"),
			gtREF),
		xmlNode("scaleOperator", 		
			attrs = c(scaleFactor = "0.75", weight = "1"),
			siteModel.alphaREF),
		xmlNode("deltaExchange",
			attrs = c(delta = "0.01", weight = "1"),
			frequenciesREF))
			
	if (clock == "strict"){
		kids2 <- list(
			xmlNode("scaleOperator", 		
				attrs = c(scaleFactor = "0.75", weight = "3"),
				clock.rateREF))
	}												else {
		kids2 <- list(
		xmlNode("scaleOperator", 		
			attrs = c(scaleFactor = "0.75", weight = "3"),
			ucld.meanREF),
		xmlNode("scaleOperator", 		
			attrs = c(scaleFactor = "0.75", weight = "3"),
			ucld.stdevREF))
	}
	
	
	
	kids3 <- list(
		xmlNode("subtreeSlide", 
			attrs = c(size = "3.2", gaussian = "true",
			weight = round(nbtax/2)), treeModelREF),
		xmlNode("narrowExchange", 							attrs = c(weight = round(nbtax/2)),
			treeModelREF),
		xmlNode("wideExchange", attrs = c(weight = 				min(c(1, round(nbtax/10)))), treeModelREF),
		xmlNode("wilsonBalding", attrs = c(weight = 			min(c(1, round(nbtax/10)))), # oder 19?
			treeModelREF),
		xmlNode("scaleOperator",
				attrs = c(scaleFactor = "0.75", weight = "3"),
				treeModel.rootHeightREF),
		xmlNode("uniformOperator",
				attrs = c(weight = round(nbtax/2)),
				treeModel.internalNodeHeightsREF)	)
	
	kids3b <- evoOperMCMCREF
	for (i in seq(along = evoOperMCMCREF))
	kids3b[[i]] <- xmlNode("scaleOperator",
		attrs = c(scaleFactor = "0.75", weight = "3"), 		kids3b[[i]])

		
	if (clock == "strict"){
		kids4 <- list(
			xmlNode("upDownOperator",
				attrs = c(scaleFactor = "0.75", 					weight = round(nbtax/2)),
				xmlNode("up", clock.rateREF),
				xmlNode("down", 									treeModel.allInternalNodeHeightsREF)))
	}												else {
		kids4 <- list(
		xmlNode("upDownOperator",
			attrs = c(scaleFactor = "0.75", 					weight = round(nbtax/2)),
			xmlNode("up",ucld.meanREF),
			xmlNode("down", treeModel.allInternalNodeHeightsREF)),
		xmlNode("swapOperator", 							attrs = c(size = "1", weight = "10",
				autoOptimize = "false"),
			branchRatesCategoriesREF),
		xmlNode("randomWalkIntegerOperator",
			attrs = c(windowSize = "1.0",
				weight = "10"),								branchRatesCategoriesREF),
		xmlNode("uniformIntegerOperator",
			attrs = c(weight = "10"),
			branchRatesCategoriesREF))
	}
				
		
	if (!nodata)
		operators <- addChildren(operators, kids = kids1)
	operators <- addChildren(operators, kids = kids2)
	operators <- addChildren(operators, kids = kids3)
	operators <- addChildren(operators, kids = kids3b)
	operators <- addChildren(operators, kids = kids4)
	
	# <mcmc> node
	# -----------
	if (length(true.monophyly) > 0){
		bl <- vector(mode = "list", length = nb.true.mono)
		for (i in 1:nb.true.mono){
			idrefmono  <- paste("monophyly(", 					names(true.monophyly)[i], ")", sep = "")
			bl[[i]] <- xmlNode("booleanLikelihood",
				xmlNode("monophylyStatistic",
					attrs = c(idref = idrefmono)))
		}
	}
	else bl <- NULL
	
	if (specModel %in% c("yule", "birthDeath")){
		jeffreyPriors <- list(
			xmlNode("oneOnXPrior", acREF),
			xmlNode("oneOnXPrior", agREF),
			xmlNode("oneOnXPrior", atREF),
			xmlNode("oneOnXPrior", cgREF),
			xmlNode("oneOnXPrior", gtREF))
	}
	else {
		jeffreyPriors <- list(
			xmlNode("oneOnXPrior", acREF),
			xmlNode("oneOnXPrior", agREF),
			xmlNode("oneOnXPrior", atREF),
			xmlNode("oneOnXPrior", cgREF),
			xmlNode("oneOnXPrior", gtREF),
			xmlNode("oneOnXPrior", evoOperMCMCREF))
	}
			
	if (!is.null(tmrcaCons)){
		tmrca  <- vector(mode = "list", length = nb.tmrca)
		for (i in 1:nb.tmrca){
		valnames <- names(tmrcaCons[[i]])
		tmrca[[i]] <- xmlNode(paste(names(tmrcaCons)[i], "Prior", 			sep = ""),
			attrs = tmrcaCons[[i]][!names(tmrcaCons[[i]]) == 				"set"],
			xmlNode("statistic", 								attrs = c(idref = 
				paste("tmrca(", tmrcaCons[[i]][valnames == "set"], 					")", sep = ""))))
		}
	}	
	prior <- xmlNode("prior", attrs = c(id = "prior"))
	prior <- addChildren(prior, kids = bl)
	prior <- addChildren(prior, kids = tmrca)
	if (!nodata)
		prior <- addChildren(prior, kids = jeffreyPriors)
	prior <- addChildren(prior, kids = list(evoLikelihoodREF))
	
	likelihood <- xmlNode("likelihood",
		attrs = c(id = "likelihood"),
		treeLikelihoodREF)
		
	# create <mcmc> <posterior> node
	# -----------------------	
	posterior <- xmlNode("posterior", attrs = c(id = "posterior"))
	
	posterior <- addChildren(posterior, 				kids = list(prior, likelihood))
	if (nodata) posterior <- removeChildren(posterior, 				kids = "likelihood")
	
	# create <mcmc> <operators> node
	# -----------------------
	operators2 <- xmlNode("operators", 					attrs = c(idref = "operators"))
	
	# create <logScreen> node
	# -----------------------
	posteriorREF <- xmlNode("posterior", 				attrs = c(idref = "posterior"))
	priorREF <- xmlNode("prior", attrs = c(idref = "prior"))
	likelihoodREF <- xmlNode("likelihood", 				attrs = c(idref = "likelihood"))
	
	logScreen <- xmlNode("log", 						attrs = c(id = "screenLog", logEvery = samplefreq),
		xmlNode("column", attrs = c(label = "Posterior", 			dp = "4", width = "12"), posteriorREF),
		xmlNode("column", attrs = c(label = "Prior", 			dp = "4", width = "12"), priorREF),
		xmlNode("column", attrs = c(label = "Likelihood", 			dp = "4", width = "12"), likelihoodREF),
		xmlNode("column", attrs = c(label = "Root Height", 			sf = "6", width = "12"), treeModel.rootHeightREF),
		xmlNode("column", attrs = c(label = "Rate", 			sf = "6", width = "12"), ucld.meanREF))
	if (nodata) logScreen <- removeChildren(logScreen, kids = 3)
	
		
		
	# create <logFile> node
	# -----------------------	
	if (clock == "strict"){
		logFile <- xmlNode("log", 
			attrs = c(id = "fileLog",
				logEvery = samplefreq, 
				fileName = logFileName),
			xmlNode("posterior", 								attrs = c(idref = "posterior")),
			xmlNode("prior", 									attrs = c(idref = "prior")),
			xmlNode("likelihood", 								attrs = c(idref = "likelihood")),
			clock.rateREF,
			treeModel.rootHeightREF
		)
		logFile <- addChildren(logFile, kids = evoOperMCMCREF)
	} 												else {
		logFile <- xmlNode("log", 
			attrs = c(id = "fileLog",
				logEvery = samplefreq, 
				fileName = logFileName),
			xmlNode("posterior", 								attrs = c(idref = "posterior")),
			xmlNode("prior", 									attrs = c(idref = "prior")),
			xmlNode("likelihood", 								attrs = c(idref = "likelihood")),
			xmlNode("rateStatistic", 								attrs = c(idref = "meanRate")),
			treeModel.rootHeightREF
		)
		logFile <- addChildren(logFile, kids = evoOperMCMCREF)
	}
		
	if (!is.null(taxonset)){
		tstat <- vector(mode = "list", length = nb.taxonset)
		for (i in 1:nb.taxonset)
			tstat[[i]] <- xmlNode("tmrcaStatistic",
				attrs = c(idref = paste("tmrca(", 				names(taxonset)[i], ")", sep = "")))
	}
	logFile <- addChildren(logFile, kids = tstat)
	
	if (clock == "strict"){
		logFile <- addChildren(logFile, 
			kids = list(evoOperMCMCREF,
				acREF, 
				agREF, 
				atREF, 				
				cgREF, 						
				gtREF, 						
				siteModel.alphaREF, 
				treeLikelihoodREF,
				evoLikelihoodREF))
	}												else {
		logFile <- addChildren(logFile, 
			kids = list(acREF, agREF, atREF, 				cgREF, 										gtREF, 
				siteModel.alphaREF, 
				ucld.meanREF, 
				ucld.stdevREF, 
				rateStatisticREF, 
				rateCovarianceStatisticREF, 
				treeLikelihoodREF,
				evoLikelihoodREF))
	}
	if (nodata)
		logFile <- removeChildren(logFile, 					kids = c("likelihood", "treeLikelihood"))
				
	
	
	# log chronograms
	# ---------------		
	logTree  <- xmlNode("logTree", 
		attrs = c(id = "treeFileLog",
			logEvery = samplefreq, 
			nexusFormat = "true",
			fileName = treeFileName,
			sortTranslationTable = "true"),
		treeModelREF,
		branchRatesREF,
		xmlNode("posterior", attrs = c(idref = "posterior")))
		
	# log phylograms
	# ---------------
	if (logSubs){
		logSubsTree  <- xmlNode("logTree", 
			attrs = c(id = "substTreeFileLog",
				logEvery = samplefreq, 
				nexusFormat = "true",
				fileName = subsTreeFileName,
				branchLengths = "substitutions"),
			treeModelREF,
			branchRatesREF
		)	
	}
	
	# assemble <MCMC> node:
	# ---------------------
	mcmc <- xmlNode("mcmc", 
		attrs = c(id = "mcmc",
			chainLength = ngen,
			autoOptimize = "true"))
	if (!logSubs)		
		mcmc <- addChildren(mcmc, 							kids = list(posterior, operators2, logScreen, logFile, 
			logTree))								else 												mcmc <- addChildren(mcmc, 							kids = list(posterior, operators2, logScreen, logFile, 
			logTree, logSubsTree))
			
			
			
	# <report> node
	# -------------
	report <- xmlNode("report", 
		xmlNode("property", attrs = c(name = "timer"),
			xmlNode("object", attrs = c(idref = "mcmc"))))
		
	
	
	
	
	
	
	
	
	
	
	# ----------------------------
	# 2:	ASSEMBLE XML FILE
	# ----------------------------
	beast <- xmlNode("beast")
	beast <- addChildren(beast, kids = list(taxa))
	if (!is.null(taxonset)) 							beast <- addChildren(beast, kids = taxset)
	
	if (clock == "strict"){
		kids1 <- list(
			alignment,
			patterns,
			ultrametricSpeciationModel,
			constantSize,
			startTree,
			treeModel,
			evoLikelihood,
			branchRates,
			gtrModel,
			siteModel, 
			treeLikelihood)
	}												else {
		kids1 <- list(
			alignment,
			patterns,
			ultrametricSpeciationModel,
			constantSize,
			startTree,
			treeModel,
			evoLikelihood,
			branchRates,
			ratestat1, 
			ratestat2,
			rateCovStat,
			gtrModel,
			siteModel, 
			treeLikelihood)
	}
	
	beast <- addChildren(beast, 
		kids = kids1)
	
	beast <- addChildren(beast, kids = yy)
	beast <- addChildren(beast, kids = monostat)
	beast <- addChildren(beast, kids = list(operators))
	beast <- addChildren(beast, kids = list(mcmc))
	beast <- addChildren(beast, kids = list(report))

	saveXML(beast, file = path)
}