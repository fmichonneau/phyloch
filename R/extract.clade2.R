extract.clade2 <- function(phy, node){
	
	# get indices of tips and internal to keep
	# -----------------------------------------
	keep <- descendants(phy, node, type = "both")
	tips2keep <- descendants(phy, node, type = "terminal")
	nodes2keep <- c(node, keep[!keep %in% tips2keep])
	
	keep <- which(phy$edge[, 2] %in% keep)
	
	# calculate correction for tip and node numbers
	# -----------------------------------------
	Ntips <- length(tips2keep)	
	tipcorrect <- min(tips2keep) - 1
	nodecorrect <- min(nodes2keep) - (Ntips + 1)
	
	# store old node number
	# -----------------------------------------
	oldnodes <- (length(phy$tip.label) + 1):(length(phy$tip.label) 		+ phy$Nnode)
	old2keep <- which(oldnodes %in% nodes2keep)
	
	# prune phylo object
	# -------------------
	phy$edge <- phy$edge[keep, ]
	phy$edge[phy$edge[, 2] <= max(tips2keep), 2] <- 		phy$edge[phy$edge[, 2] <= max(tips2keep), 2] - tipcorrect
	phy$edge[phy$edge[, 2] >= min(nodes2keep), 2] <- 		phy$edge[phy$edge[, 2] >= min(nodes2keep), 2] - 		nodecorrect
	phy$edge[phy$edge[, 1] >= min(nodes2keep), 1] <- 		phy$edge[phy$edge[, 1] >= min(nodes2keep), 1] - 		nodecorrect
	phy$Nnode <- length(nodes2keep)
	phy$edge.length <- phy$edge.length[keep]
	phy$tip.label <- phy$tip.label[tips2keep]
	phy$node.label <- phy$node.label[old2keep]
	
	# handle non-standard node-labels (e.g. from BEAST output):
	# -------------------------
	sdtnames <- c("edge", "edge.length", "Nnode", "tip.label", 		"node.label")
	if (!all(names(phy) %in% sdtnames)){
		id <- which(!names(phy) %in% sdtnames)
		for (i in id){
			phy[[i]] <- phy[[i]][old2keep]
		}
	}	
	phy
}