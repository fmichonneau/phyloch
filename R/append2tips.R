append2tips <- function (phy, tips, offset, align = FALSE, grid = FALSE,
                         col = "red", text = NULL, pch = NULL, cex, ...) {
  
  ## check tree and set tree-related paramters
  ## -----------------------------------------
  if ( !inherits(phy, "phylo") ) 
    stop("object 'phy' is not of class 'phylo'")
  ntip <- Ntip(phy)
  alltips <- 1:ntip
  if ( missing(tips) ) tips <- alltips
  
  ## parameters of previous call to plot.phylo
  ## -----------------------------------------
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  ## check if type is supported
  ## --------------------------
  if ( !lastPP$type %in% c("phylogram", "cladogram") )
    stop("currently only types 'phylogram' and 'cladogram' supported")
  
  ## set direction-related parameters
  ## --------------------------------
  dir <- lastPP$direction
  if ( dir %in% c("upwards", "downwards") )
    stop("currently only directions 'rightwards' and 'leftwards' supported")
  d.sign <- ifelse(dir %in% c("rightwards", "upwards"), 1, -1)
  minmax <- ifelse(dir %in% c("rightwards", "upwards"), 2, 1)
  adj1 <- ifelse(dir %in% c("rightwards", "upwards"), 0, 1)
  crds <- c("xx", "yy")
  crds.id <- ifelse(dir %in% c("rightwards", "leftwards"), 1, 2)
  new.list.element <- paste(crds[crds.id], "new", sep = ".")
  srt <- ifelse(dir %in% c("rightwards", "leftwards"), 0, 90)
  
  ## set missing arguments
  ## ---------------------
  if ( missing(offset) ) offset <- lastPP$label.offset
  if ( missing(cex) ) cex <- lastPP$cex
  
  # width for present tiplabels and those to append
  # -----------------------------------------------
  space1 <- lastPP$label.offset + 
    strwidth(gsub("_", " ", phy$tip.label[tips]), 
             cex = lastPP$cex, font = lastPP$font)
  space2 <- rep(0, length(tips))
  if ( !is.null(pch) ) space2 <- strwidth("O", cex = cex)
  if ( !is.null(text) ) space2 <- strwidth(text, cex = cex)
  space3 <- strwidth("-", "user", cex, lastPP$font)
  
  ## coordinates for plotting
  ## ------------------------
  if (  dir %in% c("rightwards", "upwards") ){
    gord <- 1:2
  }
  else {
    gord <- 2:1
  }
  if ( any(c("rightwards", "leftwards") %in% dir) ){
    ord <- 1:2
  }
  else {
    ord <- 2:1
    
  }
  
  ## per.xy: coordinates perpendicular to edges
  ## ---------------------------------------
  if ( is.null(lastPP[[new.list.element]]) ){
    per.xy <- lastPP[[crds[crds.id]]]
    # if ...
    labelSpace <- strwidth(phy$tip.label, 
                           cex = lastPP$cex, font = lastPP$font) +
                             lastPP$label.offset
    per.xy[alltips] <- per.xy[alltips] + d.sign * labelSpace
  } else {
    per.xy <- lastPP[[new.list.element]]
  }
  
  if ( align ) {
    per.xy.inner <- per.xy
    per.xy[tips] <- range(per.xy[tips])[minmax]
  }
  
  ## plot symbols
  ## ------------ 
  if ( !is.null(pch) ){ 
    per.xy[tips] <- per.xy[tips] + d.sign * strwidth("-", "user", cex)
    cds <- cbind(per.xy[tips], tips)[, ord]
    points(cds[, 1], cds[, 2], pch = pch, col = col, bg = col, 
           cex = cex, ...)
  }
  ## plot text
  ## ---------
  if ( !is.null(text) ){
    cds <- cbind(per.xy[tips], tips)[, ord]
    text(cds[, 1], cds[, 2], text, col = col, cex = cex, adj = c(adj1, 0.5), 
         srt = srt, ...)
    per.xy[tips] <- per.xy[tips] + d.sign * strwidth(text, "user", cex, ...)
  }
  ## plot grid
  ## ---------
  if ( grid ){
    abzug <- strwidth("-", "user", cex) * c(1, -1)# * d.sign
    for ( i in tips ) {
      xx <- c(per.xy.inner[i], per.xy[i])[gord] + abzug
      if ( diff(xx) > 0 ) lines(x = xx, y = rep(tips[i], 2), lty = 3)
    }    
  }
  
  ## update and return 'last_plot.phylo'
  ## -----------------------------------
  if ( max(per.xy) > max(lastPP[[paste(substr(crds[crds.id], 1, 1), "lim", sep = ".")]]) )
    warning("The appended text exceeds the limit of the plotting region.",
            "\n  Consider setting 'x.lim' in 'plot.phylo' to at least ", 
            round(max(per.xy), 5), ".")
  lastPP[[new.list.element]] <- per.xy
  assign("last_plot.phylo", lastPP, envir = .PlotPhyloEnv)
  invisible(lastPP)
}