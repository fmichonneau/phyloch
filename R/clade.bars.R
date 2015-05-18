clade.bars <- function(phy, nodes, bars = TRUE, barcol, barwd, 
                       text = NULL, textcol = "black", srt, 
                       cex = 1, offset, align, s = "clade", ...){
  
  ## check tree
  ## ----------
  if ( !inherits(phy, "phylo") ) 
    stop("object 'phy' is not of class 'phylo'")
  ntips <- Ntip(phy) # number of tips
  tips <- 1:ntips # numeric index of tips
  
  ## check nodes
  ## -----------
  nClades <- length(nodes)
  desNodes <- lapply(nodes, descendants, phy = phy)
  
  ## align: cladewise or all?
  ## ------------------------
  if ( missing(align) ) align <- "cladewise"
  align <- match.arg(align, c("all", "cladewise"))
  
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
  d.sign <- ifelse(dir %in% c("rightwards", "upwards"), 1, -1)
  minmax <- ifelse(dir %in% c("rightwards", "upwards"), 2, 1)
  xadj <- ifelse(dir %in% c("rightwards", "upwards"), 0, 1)
  crds <- c("xx", "yy")
  cid <- ifelse(dir %in% c("rightwards", "leftwards"), 1, 2)
  new.list.element <- paste(crds[cid], "new", sep = ".")
  
  ## string rotation
  ## ---------------
  if ( missing(srt) ) srt <- 0
  if ( srt == 0 ){
    xadj <- ifelse(dir %in% c("rightwards", "upwards"), 0, 1)
    yadj <- .5
  }
  else {
    xadj <- .5
    yadj <- ifelse(dir %in% c("rightwards", "upwards"), 0, 1)
  }  
  
  ## set 'offset' values
  ## -------------------
  if ( missing(offset) ) offset <- "-"
  if ( is.character(offset) )
    offset <- string.dim(offset, cex = lastPP$cex, font = lastPP$font,
                         srt = lastPP$srt)[, cid]
  if ( length(offset) == 1 ) offset <- rep(offset, 2)
 
  ## per.xy: coordinates perpendicular to edges
  ## ------------------------------------------
  if ( is.null(lastPP[[new.list.element]]) ){
    per.xy <- lastPP[[crds[cid]]]
    labelSpace <- string.dim(phy$tip.label, cex = lastPP$cex, 
                             font = lastPP$font, srt = lastPP$srt)[, cid]
    labelSpace <- labelSpace + lastPP$label.offset
    per.xy[tips] <- per.xy[tips] + d.sign * labelSpace
  } else {
    per.xy <- lastPP[[new.list.element]]
  }
  
  per.xy <- per.xy + d.sign * offset[1]
  
  ## extension: half the font height
  ## -------------------------------
  extension <- ifelse(dir %in% c("rightwards", "leftwards"),
                      strheight("A", cex = lastPP$cex, font = lastPP$font),
                      strwidth("A", cex = lastPP$cex, font = lastPP$font)) 
  extension <- extension * .5
  
  ## calculate length of bars
  ## ------------------------
  getBarLength <- function(x, extension = extension, phy){
    x <- range(descendants(phy, x))
    if ( length(x) == 0 ) x <- x + c(-extension, extension) # single tips!
    x + c(-extension, extension)
  } 
  barLength <- lapply(nodes, getBarLength, 
                      extension = extension, phy = phy)
  barLength <- do.call(rbind, barLength)
  
  ## calculate perpendicular position of bars
  ## ----------------------------------------
  getCladePerXY <- function(x, per.xy, minmax){
    range(per.xy[x])[minmax] 	
  } 
  clade.per.xy <- sapply(desNodes, getCladePerXY, per.xy = per.xy, 
                         minmax = minmax)
  if ( align == "all" ) {
    mostExtremeTip <- range(clade.per.xy)[minmax]
    clade.per.xy <- mostExtremeTip
  }
  
  xycoords <- data.frame(clade.per.xy, clade.per.xy, # xx
                         barLength[, 1], barLength[, 2]) # yy
  if ( dir %in% c("upwards", "downwards") )
    xycoords <- xycoords[, c(3, 4, 1, 2)]
  
  ## plot bars:
  ## ---------------
  if ( bars ) {
    ## set bar colors
    if ( missing(barcol) ) barcol <- "grey"
    if ( length(barcol) < nClades ) 
      barcol <- rep(barcol, ceiling(nClades / length(barcol)))
    ## set bar width
    if ( missing(barwd) ) barwd <- 5
    if ( length(barwd) < nClades )
      barwd <- rep(barwd, ceiling(nClades / length(barwd)))
    for ( i in 1:nrow(xycoords) ){
      lines(xycoords[i, 1:2], xycoords[i, 3:4],   			 
            lwd = barwd[i], col = barcol[i], ...)
    }  
  }    
  
  ## plot text
  ## ---------
  if ( !is.null(text) ){
    center <- rowMeans(barLength) # center text
    texlen <- length(text)
    if ( length(cex) != texlen ) cex <- rep(cex, texlen)
    xycoords[, 1] <- xycoords[, 1] + offset[2] * d.sign
    
    for ( i in seq_along(text) )
      text(xycoords[i, 1], center[i], text[i], adj = c(xadj, yadj), srt = srt,
           cex = cex[i], col = textcol[i])
#         thistxt <- unlist(strsplit(text[i], "\n"))
#         if ( length(thistxt) == 1 )
#           mixed.fonts.text(x = xycoords[i, 1], # - xs, 
#                            y = center[i], 
#                            txt = thistxt, cex = cex[i], 
#                            col = textcol, 
#                            s = s)							
#         else {
#           ys <- strheight("O", cex = cex[i]) * 0.75
#           mixed.fonts.text(x = xycoords[i, 1] - xs, 
#                            y = center[i] + ys, 
#                            txt = thistxt[1], cex = cex[i], 
#                            col = textcol, 
#                            s = s)
#           mixed.fonts.text(x = xycoords[i, 1] - xs, 
#                            y = center[i] - ys, 
#                            txt = thistxt[2], cex = cex[i], 
#                            col = textcol, 
#                            s = s)		
#         }
#     }
#    }	## end of FOR loop
    ## add text space to xycoords
    ## --------------------------
    sh <- string.dim(s = text, cex = cex, srt = srt)[, cid] * d.sign
    for ( i in seq_along(desNodes)){
      xycoords[desNodes[[i]], 1:2] <- xycoords[desNodes[[i]], 1:2] + sh
    }
  }
  
  # ------- FAN -------------------
  if ( lastPP$type == "fan" ){
    u <- par("usr")
    user.asp <- diff(u[3:4])/diff(u[1:2])
    p <- par("pin")
    inches.asp <- p[2]/p[1]
    asp <- user.asp/inches.asp
    
    nodes <- lapply(nodes, descendants, phy = phy)
    nn <- sapply(nodes, length)
    
    # Radius:
    # -------
    getRadius <- function(id, lPP)
      max(sqrt(lPP$xx[id]^2 + lPP$yy[id]^2))
    radius <- sapply(nodes, getRadius, lPP = lastPP)
    radius <- radius + offset[1]
    
    ang <- (nn - 1) / ntips
    x <- cbind(ang, 1 - ang)
    x <- apply(x, 1, function(x) c(0, cumsum(x)/sum(x)))
    dx <- apply(x, 2, diff)
    
    # starting position
    # -----------------
    startpos <- vector(length = length(nodes))
    for (i in seq(along = nodes))
      startpos[i] <- (min(nodes[[i]]) - 1) * 2 * pi/ntips
    
    # angles for text labels:
    # ----------------------
    ang <- ang * 360 / 2
    for (i in seq(along = nodes))
      ang[i] <- ang[i] + (min(nodes[[i]]) - 1) / ntips * 360
    id <- which(ang > 90 & ang < 270)
    ang[id] <- ang[id] + 180
    pos <- rep(4, length(ang))
    pos[id] <- 2
    
    n <-  apply(dx, 2, function(dx) 
      max(2, floor(200 * dx[1])))
    n <- rep(61, length(nodes))
    
    # ajust 'textor' and 'cex'
    # ------------------------
    texlen <- length(text)
    if ( length(textor) != texlen )
      textor <- rep(textor, texlen)
    if ( length(cex) != texlen )
      cex <- rep(cex, texlen)
    
    # loop over nodes to draw bars and text
    # -------------------------------------
    for ( i in seq(along = nodes) ){
      t2p <- 2 * pi * seq(x[1, i], x[2, i], 
                          length = n[i]) + startpos[i]
      xcc <- cos(t2p) * radius[i]
      ycc <- sin(t2p) * radius[i] * asp
      lines(xcc, ycc, col = barcol[i], lwd = barwd, 
            lend = 2, ...)	
      
      # text on fan-shaped phylogeny
      # ----------------------------	
      if(!is.null(text)){	
        if (textor[i] == "t"){
          letter <- unlist(strsplit(text[i], ""))
          nlett <- length(letter)
          t2p <- 2 * pi * seq(x[1, i], x[2, i], 
                              length = nlett) + startpos[i]
          xcc <- rev(cos(t2p) * radius[i])
          ycc <- rev(sin(t2p) * radius[i] * asp)
          tang <- (t2p * 180 / pi) - 90
          for ( j in seq(along = letter) )
            text(xcc[j], ycc[j], letter[j], 
                 cex = cex[i], col = textcol, 
                 adj = c(.5, .5), srt = tang[j], ...)
        }
        if ( textor[i] == "r" ){
          id <- 31#n[i]/2 + 1
          text(xcc[id], ycc[id], text[i], 
               cex = cex[i], col = textcol, 
               pos = pos[i], srt = ang[i], ...)
        }
        if ( textor[i] == "v" )
          warning("not yet implemented")
#           text(xx[i], center[i], text[i], 
#                cex = cex[i], col = textcol, 
#                adj = c(.5, .5), srt = 90, ...)
      }
    }
  }
  ## update and return 'last_plot.phylo'
  ## -----------------------------------
  new <- lastPP[[crds[cid]]]
  for ( i in seq_along(desNodes) ) {
    new[desNodes[[i]]] <- xycoords[i, 1 + cid]
  }
  lastPP[[new.list.element]] <- new
  assign("last_plot.phylo", lastPP, envir = .PlotPhyloEnv)
  invisible(lastPP)  
}
