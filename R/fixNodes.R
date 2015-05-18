fixNodes <- function(phy, quiet = TRUE){
  
  # local functions
  # ---------------
  swop.nodes <- function(x, phy){
    i1 <- which(phy$edge == x[1], arr.ind = TRUE)
    i2 <- which(phy$edge == x[2], arr.ind = TRUE)
    phy$edge[i1] <- x[2]
    phy$edge[i2] <- x[1]
    phy
  }
  
  ## parameters
  ## ----------
  nt <- Ntip(phy)
  nn <- Nnode(phy)
  int <- nt + 1:nn
  root <- min(int)
  unfixed <- phy
  
  ## fix tips
  ## --------
  new <- 1:nt
  x <- phy$edge
  id <- x[, 2] %in% new
  old <- x[id, 2]
  x[id, 2] <- new
  phy$edge <- x
  phy$tip.label <- phy$tip.label[old]	
  
  #bigtree(phy)
  
  ## fix internal nodes
  ## loop over internal nodes beginning at the root
  ## ----------------------------------------------
  for ( i in int ){
    if ( !quiet ) cat("\nnode: ", i)
    ## old: daughter nodes according to the old enumeration
    old <- new <-phy$edge[which(phy$edge[, 1] == i), 2]
    nn <- lapply(old, descendants, phy = phy, type = "i")
    nn <- sapply(nn, length)
    isTip <- old < root
    #
    if ( all(isTip) ) {
      if ( !quiet ) cat(" nothing to do")
    } else {
      step <- 1
      for ( j in seq_along(isTip) ) {
        if ( !isTip[j] ) {
          new[j] <- i + step
          step <- step + 1 + nn[j]
        }
      } 
    }
    ## tab: matrix of 2 columns: old and new
    tab <- matrix(c(old, new), ncol = 2)
    ## delete tips from tab
    tip <- which(tab[, 1] < root)
    if ( length(tip) > 0 ){
      ## and also the rows where the new node from a tip is present
      tip <- union(tip, which(tab[tip, 2] == tab[, 1]))
      tab <- tab[-tip, , drop = FALSE]
    }
    
    ## choose only rows that lead to actual change
    if ( nrow(tab) > 0 ) tab <- tab[tab[, 1] != tab[, 2], , drop = FALSE]
    ## not sure, what this actually does
    if ( nrow(tab) > 0 ){
      for ( l in seq_along(tab[-1, 1]) ){
        ll <- tab[l, 2] == tab[, 1]
        if ( any(ll) ){
          tab[ll, 1] <- tab[l, 1]
        }
      }
      
      if ( !quiet ) print(tab)
      # exchange the new and the old node numbers
      for ( k in 1:nrow(tab) ){
        phy <- swop.nodes(tab[k, ], phy)
      }
    }
  }
    
  # fix <node.label> element
  # ------------------------
  id <- node.trans(source = unfixed, target = phy, index = TRUE)
  sdtnames <- c("edge", "edge.length", "Nnode", "tip.label")
  if ( !all(names(phy) %in% sdtnames) ){
    nls <- which(!names(phy) %in% sdtnames)
    for ( i in nls )
      phy[[i]] <- phy[[i]][id]
  }	
  phy
}