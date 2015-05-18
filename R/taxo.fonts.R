taxo.fonts <- 
function(phy, cex, offset, col, align = FALSE, auto.correction = 
    TRUE, s = c("spec.", "subspec.", "var.", "cf.", "agg.", "syn."),
    lty = 3, lcol = "grey25"
    )
{
	lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
	tax <- phy$tip.label
	tips <- 1:length(tax)
	
	# color + cex
	# -----------
	if (missing(col)) col <- rep("black", length(tax))
	if (length(col) != length(tax)) col <- rep(col, length(tax))
	
	if (missing(cex)) cex <- lastPP$cex
	
	# correct and adjust tip labels
	# -----------------------------
	if (auto.correction){
		tax <- gsub("var_", "var._", tax, fixed = TRUE)
		tax <- gsub("spec_", "spec._", tax, fixed = TRUE)
		tax <- gsub("ssp_", "ssp._", tax, fixed = TRUE)
		tax <- gsub("sp_[.]*|sp$", "spec._", tax)
	}
	tax <- gsub("_", " ", tax)
	
	# coordinates
	# -----------
	if (missing(offset)) 
		offset <- lastPP$label.offset
	if (lastPP$dir == "rightwards"){
	    xx <- lastPP$xx[tips] + offset
	    if (align)
	        xx <- rep(max(xx), length(xx))
	    yy <- lastPP$yy[tips]
	}
	if (lastPP$dir == "leftwards"){
	    xx <- lastPP$xx[tips] - offset
	    xx <- xx - strwidth(tax, font = 3, cex = cex)
	    if (align) {
	    	xx <- min(lastPP$xx[tips] - offset)
	    	xx <- xx - strwidth(tax, font = 3, cex = cex)
	    	xx <- rep(min(xx), length(xx))
	    }	  
	    yy <- lastPP$yy[tips]      
	}
	if (lastPP$dir == "upwards"){
	    yy <- lastPP$yy[tips] + offset
	    if (align)
	        yy <- rep(max(yy), length(yy))
	    xx <- lastPP$xx[tips]
	}
	if (lastPP$dir == "downwards"){
	    yy <- lastPP$yy[tips] - offset
	    if (align) {
	    	yy <- rep(min(yy), length(yy))
	    }	  
	    xx <- lastPP$xx[tips]      
	}
	
	
	
	
	# identify tips that contain 's'
	# ------------------------------
	s1 <- gsub("[.]", ".", paste(s, collapse = "|"))
	id <- grep(s1, tax)
	
	if (length(id) > 0){
	
	# prepare 'space' vector
	space <- vector(length = lastPP$Ntip)
	
	# plot unmodified tips
	# --------------------
	text(xx[-id], yy[-id], tax[-id], font = 3, adj = c(0, 0.5), 
		cex = cex, col = col[-id])
	space[-id] <- strwidth(tax[-id], font = 3, cex = cex)
	
	# plot modified tips
	# ------------------
	for (i in id){
		tl <- tax[i]
		x <- xx[i]
		y <- yy[i]
		for (j in seq(along = s)){
			if (length(grep(s[j], tl)) > 0){
				tl <- unlist(strsplit(tl, s[j]))
				# first part of name
				text(x, y, tl[1], font = 3, cex = cex, 
					adj = c(0, 0.5), col = col[i])
				sp1 <- strwidth(tl[1], font = 3, cex = cex)
					
				x <- x + strwidth(tl[1], cex = cex)
				text(x, y, s[j], font = 1, cex = cex, 
					adj = c(0, 0.5), col = col[i])
				sp2 <- strwidth(s[j], font = 1, cex = cex)
					
				# second part of name
				if (length(tl) > 1){
					x <- x + strwidth(s[j], cex = cex)
					text(x, y, tl[2], font = 3, cex = cex, 
						adj = c(0, 0.5), col = col[i])
					sp3 <-	strwidth(tl[2], font = 3, cex = cex)
				}												else sp3 <- 0
				sp <- sp1 + sp2 + sp3 
			}			
		}
		space[i] <- sp		
	}
	
	}												else { # length(id) == 0
		space <- strwidth(tax, font = 3, cex = cex)
		if (lastPP$dir %in% c("rightwards", "leftwards"))
		text(xx, yy, tax, font = 3, adj = c(0, 0.5), 
		    cex = cex, col = col)
		if (lastPP$dir == "upwards")
		    text(xx, yy, tax, font = 3, adj = c(0, 0.5),  
		        srt = 90, cex = cex, col = col)
		if (lastPP$dir == "downwards")
		    text(xx, yy, tax, font = 3, adj = c(0, 0.5),  
		        srt = 270, cex = cex, col = col)
	}
	
	# plot dotted lines
	# -----------------
	if (align){
		o <- strwidth("o", cex = cex)
		if (lastPP$dir == "rightwards"){
		    lxy <- cbind(lastPP$xx[tips] + o, xx - o, yy, yy)
		    id <- which(lxy[, 2] - lxy[, 1] > 0)
		}    
	    if (lastPP$dir == "leftwards"){
		    lxy <- cbind(xx + (space + o), 
		        lastPP$xx[tips] - o, yy, yy)
		    id <- which(lxy[, 2] - lxy[, 1] > 0)    
	    }
	    o  <-  0
	    if (lastPP$dir == "upwards"){
		    lxy <- cbind(xx, xx, lastPP$yy[tips] + o, 
		        yy - o)
		    id <- which(lxy[, 4] - lxy[, 3] > 0)    
	    }
	    if (lastPP$dir == "downwards"){
		    lxy <- cbind(xx, xx, lastPP$yy[tips] - o, yy + o)
		    id <- which(lxy[, 3] - lxy[, 4] > 0)    
	    }
	    for (i in id)
	        lines(lxy[i,1:2], lxy[i,3:4], col = lcol, lty = lty)

	}
		
	# update and return 'last_plot.phylo':
    # ------------------------------------
    new.x <- xx + space
	assign("last_plot.phylo", 
    	c(lastPP, list(new.xx = new.x)), 
        envir = .PlotPhyloEnv)
    invisible(lastPP)
}