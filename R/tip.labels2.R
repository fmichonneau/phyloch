tiplabels2 <- function (text, tip, adj = c(0.5, 0.5), frame = "rect", pch = NULL, 
    thermo = NULL, pie = NULL, piecol = NULL, col = "black", 
    bg = "yellow", label.offset = 0, ...) 
{
    lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    if (missing(tip)) 
        tip <- 1:lastPP$Ntip
    XX <- lastPP$xx[tip] + label.offset
    YY <- lastPP$yy[tip]
    BOTHlabels(text, tip, XX, YY, adj, frame, pch, thermo, pie, 
        piecol, col, bg, ...)
}