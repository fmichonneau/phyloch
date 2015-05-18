add.geoscale <- 
function(phy, alpha = 0.5, ...){

	ntips <- length(phy$tip.label)
	rootage <- branching.times(phy)[1]
	
	age <- list(
		holocene = c(0, 0.0115),
		pleistocene = c(0.0115, 1.806),
		pliocene = c(1.806, 5.332),
		miocene = c(5.332, 23.03),
		oligocene = c(23.03, 33.9),
		eocene = c(33.9, 55.8),
		paleocene = c(55.8, 65.5),
		creataceous = c(65.5, 145.5),
		jurassic = c(145.5, 199.6),
		triassic = c(199.6, 251.0)
	)
	
	scale <- function(age, rootage){
		age <- rootage - age
		age
	}
	age <- lapply(age, scale, rootage = rootage)
	
	ext <- function(age) {
		if (age[1] > 0)
			x <- TRUE									
	    else x <- FALSE
	}
	ind <- which(unlist(lapply(age, ext)))
	age <- age[ind]
	
	
	# col <- terrain.colors(length(age))
	col <- terrain.colors(length(age), alpha = alpha)
	for (i in seq(along = age)){
		age[[i]] <- c(age[[i]], col[i])
		}
	
	yy <- c(1, ntips)
	# adjust overshoot
	# ----------------
	yext <- 0.01 * ntips
	yy[1] <- yy[1] - yext # ybottom
	yy[2] <- yy[2] + yext # ytop
	
	# plot line ...
	epoch <- function(x, yy, ...){
		xleft <- as.numeric(x[2])
		xright <- as.numeric(x[1])
		rect(xleft, yy[1], xright, yy[2], col = x[3], 
		    border = NA, ...)
	}
	lapply(age, epoch, yy = yy)
	
	# textpoition
	#textpos <- function(x){
	#	x <- mean(as.numeric(x[1:2]))
	#	x
	#}
	#textx <- unlist(lapply(age, textpos))
	#text(textx[3:7], yy[2] * 1.01, c("Pli.", "Miocene", "Oligo.", "Eocene", "Paleocene"), cex = 1)
}
