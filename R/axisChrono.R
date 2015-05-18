axisChrono <- 
function (side = 1, unit = "Ma", fact = 1, ...) 
{
    lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    if (lastPP$type %in% c("phylogram", "cladogram")) {
        if (lastPP$direction %in% c("rightwards", "leftwards")) {
            x <- pretty(lastPP$xx)
            if (lastPP$direction == "rightwards") 
                maxi <- max(lastPP$xx)
            else {
                maxi <- min(lastPP$xx)
                x <- -x
            }
        }
        else {
            x <- pretty(lastPP$yy)
            if (lastPP$direction == "upwards") 
                maxi <- max(lastPP$yy)
            else {
                maxi <- min(lastPP$yy)
                x <- -x
            }
        }
    }
    axis(side = side, at = c(maxi - x), labels = 
        abs(x * fact), ...)
    mtext(text = unit, side = side, at = 1.07 * maxi, line = 1, ...)
}