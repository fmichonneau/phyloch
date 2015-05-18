noi <- function(phy, group, regex = FALSE){
	
	# test to filter out trees with non-consecutive tip labels
	# --------------------------------------------------------
	canonical <- seq_along(phy$tip.label)
	given <- phy$edge[, 2][phy$edge[, 2] %in% canonical]
	given <- as.integer(given)
	if (!identical(canonical, given)) 
	  stop("tips are not numbered consecutively.",
	       " Type '?fixNodes' for help.")
	
	# core function
	# -------------
	get.mrca <- function(phy, group, regex){
	  # apply regular expression matching
	  # ---------------------------------
    if ( regex ) {
      group <- phy$tip.label[grep(paste(group, collapse = "|"), phy$tip.label)]  
    }
    nn <- which(phy$tip.label %in% group)
    if ( length(nn) > 1) {
      repeat {
        nn <- sort(unique(phy$edge[phy$edge[, 2] %in% nn, 1]))
        gg <- phy$tip.label[descendants(phy, min(nn))]
        if ( all(group %in% gg) ) break
      } 
    }
    min(nn)
	} # end of get.mrca
	
	# turn 'group' to list
	if ( !is.list(group) ) group <- list(group)
  
  ## turn factor to character strings
  ## --------------------------------
  if ( is.factor(group[[1]]) ){
    defactor <- function(x) levels(x)[x]
    group <- lapply(group, defactor)
  }
  
	# convert tip numbers to tip labels
	# ---------------------------------
	if ( mode(group[[1]]) == "numeric" & is.null(regex) )
	  group <- lapply(group, function(x, phy) 
	    phy$tip.label[x], phy = phy)
  
 	# check tip labels
	# ----------------
	chk <- setdiff(unlist(group), phy$tip.label)
	if ( length(chk) > 0 & is.null(regex) )						
	    stop(paste(chk, collapse = "\n"), 
	        "\nare not valid tip labels.")
		
	sapply(group, get.mrca, phy = phy, regex = regex)
}
