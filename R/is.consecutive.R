is.consecutive <- function(phy){
  
  ## check tree
  ## ----------
  if ( !inherits(phy, "phylo") ) 
    stop("object 'phy' is not of class 'phylo'")
  
  tips <- 1:Ntip(phy)
  actual.tips <- phy$edge[phy$edge[, 2] %in% tips, 2]
  identical(tips, actual.tips)
}