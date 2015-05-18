tip.color <- function(phy, tips, regex = FALSE, col, bgcol){
	
	# checks and definitions
	# ----------------------
	if (!inherits(phy, "phylo"))
		stop("'phy' is not of class 'phylo'")
	if (missing(bgcol)) bgcol <- "black"
	tiplab <- phy$tip.label
	nbtips <- length(tiplab)
	tipcol <- rep(bgcol, nbtips)
		
	if (!is.list(tips)) tips <- list(tips)
	
	for (i in seq(along = tips)){
		pattern <- paste(tips[[i]], collapse = "|")
		tipcol[grep(pattern, tiplab, fixed = FALSE)] <- col[i]
	}
	tipcol
}
