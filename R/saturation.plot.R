saturation.plot <- function (x, proxy = c("JC69", "K80", "F81", "K81", "F84", "TN93"), gamma = FALSE, col = "red") {

	par(mfrow = c(2, 3))
	for (i in seq(along = proxy)){
		raw <- dist.dna(x, model = "raw", pairwise.deletion = TRUE)		
		markov <- dist.dna(x, model = proxy[i], gamma = gamma,
			pairwise.deletion = TRUE)
		raw <- as.vector(raw)
		markov <- as.vector(markov)
		

		ind <- union(which(raw == "NaN"), which(markov == "NaN"))
		raw <- raw[-ind]
		markov <- markov[-ind]
		raw <- raw[-which(raw == max(raw))]
		markov <- markov[-which(markov == max(markov))]
		raw <- raw[order(markov)]
		markov <- markov[order(markov)]
		plot(markov, raw, ylab = "uncorrected pairwise difference",
			xlab = "corrected pairwise difference",
		 	col = col, main = proxy[i])
		
		# MAD
		X <- raw - markov
		MAD <- round(mad(X), digits = 6)
		ypos <- 0.95 * (max(raw) - min(raw))
		xpos <- 0.25 * (max(markov) - min(markov))
		
		text(xpos, ypos, paste("MAD = ", MAD))
		mad <- 1.4826 * median(abs(X - median(X)))
		cat("\n", proxy[i], ": MAD = ", mad, sep = "")
	}
}