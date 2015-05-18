treeannotator <- function(input = "/Users/Stoffi/R_files/BEAST/gigbeast.trees", limit = 0.9, exe = "/Applications/BEAST.v1.4.7/bin"){
	
	# check correctness of input path:
	if(!file.exists(input))
		stop("The input file does not exist.", call. = FALSE)
	
	filename <- tail(unlist(strsplit(input, "/")), 1)
	datapath <- gsub(filename, "", input)
	filename <- head(unlist(strsplit(input, "\\.")), 1)
	output <- paste(filename, "con", sep = ".")	

	setwd(exe)
	limit <- paste("-limit", limit)
	call <- paste("./treeannotator", limit, input, output)
	system(call)
	setwd(datapath)
	tr <- read.nexus(output)
	tr
}