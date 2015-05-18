edge.color <- function(phy, groups, what = "crown", col, bgcol){
	
	if (missing(bgcol)) bgcol <- "black"
	if (missing(col)) col <- "red"
	
	# number of groups:
	# -----------------
  if (is.character(groups)) groups <- list(groups)
	n <- ifelse(is.matrix(groups), nrow(groups), length(groups))
	col <- rep(col, length.out = n)
	
	what <- match.arg(what, c("crowngroup", "stemgroup"))
	stem <- ifelse(what == "stemgroup", TRUE, FALSE)
	
  # create vector of edge colors and fill with 'bgcol'
  # --------------------------------------------------
	ecol <- rep(bgcol, nrow(phy$edge))
  
	mrcas <- mrca(phy)
	tab2mrca <- function(x, mrcas, phy){
    if (is.monophyletic(phy, x)){
      mrcas[rownames(mrcas) == x[1], colnames(mrcas) == x[2]]
    }
    else {
      MRCA <- noi(phy, groups)
      tip.id <- which(phy$tip.label %in% groups)
      
      edge.id <- vector()
      for (i in seq_along(tip.id)){
        outer <- inner <- tip.id[i]
        while (inner != MRCA){
          inner <- phy$edge[phy$edge[, 2] == outer[1], 1]
          outer <- c(inner, outer)
        }
        edge.id <- c(edge.id, which(phy$edge[, 2] %in% outer))
      }
      unique(edge.id)
    }
	}
	if (is.matrix(groups)) {
	  groups <- apply(groups, 1, tab2mrca, mrcas = mrcas, phy = phy)
	}
	if (is.numeric(groups)) {
	  id <- lapply(groups, descendants, phy = phy, type = "t")
	}					
  else {
    id <- groups
  }
	
	# assemble colors vectors:
	# ------------------------
	for (i in seq_along(id)){	
	  ind <- which.edge(phy, id[[i]])
	  if (stem && length(ind) > 1) ind <- c(ind, min(ind) - 1)
	  ecol[ind] <- col[i]
	}
	ecol
}