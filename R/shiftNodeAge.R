shiftNodeAge <- function(phy, node, shift = NULL, age = NULL){
	
	# check input data
	# ----------------
	if (!inherits(phy, "phylo")) 
        stop("Object \"phy\" is not of class \"phylo\".")
        
    if (!is.ultrametric(phy)) 
        stop("Object \"phy\" is not ultrametric.")
        
    ntips <- length(phy$tip.label)
    nint <- phy$Nnode # number of internal nodes
    inodes <- (ntips + 1):(ntips + nint) # internal nodes
    if (!node %in% inodes) 
        stop(paste("Object \"node\" is not an internal node", 
        	"of \"phy\"."))
        
    dnodes <- phy$edge[phy$edge[, 1] == node, 2] # desc. nodes
    edg <- which.edge(phy, node) # edge
    dedgs <- sapply(dnodes, which.edge, phy = phy) # desc. edges
    
    if (!is.null(age)){
    	xx <- numeric(ntips + nint)
    	for (i in 1:dim(phy$edge)[1]) xx[phy$edge[i, 2]] <- 
			xx[phy$edge[i,1]] + phy$edge.length[i]
		rootage <- max(xx)
		nodeage <- rootage - xx[node]
		shift <- age - nodeage
		if (phy$edge.length[edg] < shift)
    	stop(paste("\"age\" exceeds the", 					"corresponding branch length."))
    	
    }
    
    if (phy$edge.length[edg] < shift)
    	stop(paste("Length of \"shift\" exceeds the", 			"corresponding branch length."))
    phy$edge.length[edg] <- phy$edge.length[edg] - shift
    phy$edge.length[dedgs] <- phy$edge.length[dedgs] + shift
    
    phy   
}