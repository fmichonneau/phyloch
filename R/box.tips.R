box.tips <- function(phy, nodes, col, border = NA, text = NULL, 
                     textcol = "black", cex = 1, rshift = 0, 
                     align, expand = c(0, 0, 0, 0)){
  
  ## check tree
  ## ----------
  if ( !inherits(phy, "phylo") ) 
    stop("object 'phy' is not of class 'phylo'")
  ntips <- Ntip(phy)
  tips <- 1:ntips
  
  ## check nodes
  ## -----------
  nClades <- length(nodes)
  desNodes <- lapply(nodes, descendants, phy = phy)
  ntpg <- sapply(desNodes, length) # number of tips per group
  boxtips <- unlist(desNodes)
  
  ## parameters of previous call to plot.phylo
  ## -----------------------------------------
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  ## check if type is supported
  ## --------------------------
  if ( !lastPP$type %in% c("phylogram", "cladogram"))
    stop("currently only types 'phylogram' and 'cladogram' supported")
  
  ## set direction-related parameters
  ## --------------------------------
  dir <- lastPP$direction
#   if ( !dir %in% c("rightwards", "leftwards", "upwards") )
#     stop("currently only directions 'rightwards' and 'leftwards' supported")
  
  d.sign <- ifelse(dir %in% c("rightwards", "upwards"), 1, -1)
  minmax <- ifelse(dir %in% c("rightwards", "upwards"), 2, 1)
  crds <- c("xx", "yy")
  cid <- ifelse(dir %in% c("rightwards", "leftwards"), 1, 2)
  new.list.element <- paste(crds[cid], "new", sep = ".")
  
  hlo <- lastPP$label.offset * .5 # half label offset
  if ( missing(align) ) align <- "tipwise"
  
  
  ## calculate 'inner' margin of boxes
  ## ---------------------------------
  inner <- lastPP[[crds[cid]]][tips]
  
  ## calculate 'outer' margin of boxes
  ## -------------------------------
  outer <- string.dim(phy$tip.label, cex = lastPP$cex, 
                      font = lastPP$font, srt = lastPP$srt)[, cid]
  outer <- outer + lastPP$label.offset
  outer <- inner + d.sign * outer
  
  if ( align == "cladewise" ) 
    for ( i in seq_along(desNodes) ){
      i <- desNodes[[i]]
      outer[i] <- rep(range(outer[i])[minmax], length(i))  
    }
  if ( align == "all" ) 
    outer[boxtips] <- rep(range(outer)[minmax], length(boxtips))
  
  ## calculate 'vertical' margin of boxes
  ## ------------------------------------
  lower <- boxtips - .5
  upper <- boxtips + .5
  
  ## calculate coordinates for placement of boxes
  ## --------------------------------------------
  #id <- match(sort(boxtips), boxtips)
  #xy <- cbind(inner[id], lower, outer[id], upper)
  xy <- cbind(inner[boxtips], lower, outer[boxtips], upper)
  if ( dir %in% c("upwards", "downwards") )
    xy <- xy[, 4:1]
  colnames(xy) <- c("xleft", "ybottom", "xright", "ytop")
  
  ## plot boxes
  ## ----------
  if ( missing(col) ) col <- "lightgrey"
  if ( length(col) < nClades )
    col <- rep(col, ceiling(nClades / length(col)))
  col <- rep(col, ntpg)
  for ( i in 1:nrow(xy) )
    polygon(x = xy[i, c(1, 3, 3, 1)], y = xy[i, c(2, 2, 4, 4)], 
            col = col[i], border = border[i])
  
  # plot text
  # ---------
  center <- sapply(cladeHeight, mean)
  if ( !is.null(text) ){
    xy[, 1] <- xy[, 3] + lastPP$label.offset * .5
    xy[, 3] <- xy[, 1] + strwidth(text, cex = cex) + 
      lastPP$label.offset
    rect(xy[, 1], xy[, 2], xy[, 3], xy[, 4], col = col, 
         border = border)
    text(xy[, 1] + 0.5 * (xy[, 3] - xy[, 1]), 
         center, text, cex = cex, adj = c(0.5, 0.5), 
         col = textcol)
  }
  
  ## update and return 'last_plot.phylo'
  ## -----------------------------------
  # 	new <- lastPP[[crds[cid]]]
  # 	for ( i in seq_along(desNodes) ) {
  # 	  new[desNodes[[i]]] <- xycoords[i, 1]
  # 	}
  lastPP[[new.list.element]] <- outer
  assign("last_plot.phylo", lastPP, envir = .PlotPhyloEnv)
  invisible(lastPP)
}
