add.scale.bar2 <- function (x, y, length = NULL, unit = NULL, ...) 
{
    lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    direc <- lastPP$direction
    if (is.null(length)) {
        nb.digit <- if (direc %in% c("rightwards", "leftwards")) 
            diff(range(lastPP$xx))
        else diff(range(lastPP$yy))
        nb.digit <- ceiling(log10(nb.digit)) - 2
        length <- eval(parse(text = paste("1e", nb.digit, sep = "")))
    }
    if (missing(x) || missing(y)) 
        switch(direc, rightwards = {
            x <- 0
            y <- 1
        }, leftwards = {
            x <- max(lastPP$xx)
            y <- 1
        }, upwards = {
            x <- max(lastPP$xx)
            y <- 0
        }, downwards = {
            x <- 1
            y <- max(lastPP$yy)
        })
        
    label <- paste(as.character(length), unit)
    offset <- strwidth("0", ...) * 0.5
    switch(direc, rightwards = {
        segments(x, y, x + length, y)
        text(x + length + offset, y, label, adj = c(0, 
            0.5), ...)
    }, leftwards = {
        segments(x - length, y, x, y)
        text(x - (length + offset), y, label, adj = c(1, 
            0.5), ...)
    }, upwards = {
        segments(x, y, x, y + length)
        text(x, y + length + offset, label, adj = c(0, 
            0.5), srt = 90, ...)
    }, downwards = {
        segments(x, y - length, x, y)
        text(x, y - (length + offset), label, adj = c(0, 
            0.5), srt = 270, ...)
    })
}