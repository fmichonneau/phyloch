locpie <- 
function (xpos, ypos, x, edges = 200, radius = 1, col, 
    startpos = 0, border, ...) 
{
    u <- par("usr")
    user.asp <- diff(u[3:4])/diff(u[1:2])
    p <- par("pin")
    inches.asp <- p[2]/p[1]
    asp <- user.asp/inches.asp
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
   
    bc <- 2 * pi * (x[1:nx] + dx/2) + startpos
    for (i in 1:nx) {
        n <- max(2, floor(edges * dx[i]))
        t2p <- 2 * pi * seq(x[i], x[i + 1], length = n) +
        	startpos
        xc <- c(cos(t2p) * radius + xpos, xpos)
        #print(xc)
        yc <- c(sin(t2p) * radius * asp + ypos, ypos)
        #print(yc)
        polygon(xc, yc,  col = col[i], border = border, ...)
    }
}