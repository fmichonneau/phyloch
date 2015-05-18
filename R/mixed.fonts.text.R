mixed.fonts.text <- 
function(x, y, txt, s, cex, col, font1 = 3, font2 = 1){
	
	# identify tips that contain 's'
	# ------------------------------
	s1 <- paste(s, collapse = "|")
	mixed <- grep(s1, txt)
	
	# plot non-mixed text
	# -------------------
	if (length(mixed) == 0)
	text(x, y, txt, font = font1, adj = c(0, 0.5), 
		cex = cex, col = col)
	
	# plot mixed text
	# ---------------
	for (j in seq(along = s)){
		if (length(grep(s[j], txt)) > 0){
			txt <- unlist(strsplit(txt, s[j]))
			text(x, y, txt[1], font = font1, cex = cex, 
				adj = c(0, 0.5), col = col)
			x <- x + strwidth(txt[1], cex = cex)
			text(x, y, s[j], font = font2,
					cex = cex, adj = c(0, 0.5), col = col)
			x <- x + strwidth(s[j], cex = cex)
			text(x, y, txt[2], font = font1,
					cex = cex, adj = c(0, 0.5), col = col)
		}
	}				
}