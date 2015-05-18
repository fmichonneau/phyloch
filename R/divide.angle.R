divide.angle <-
function(n, a = 180){
        x <- a/(n + 1) # sector
        y <- (180 - a)/2
        stop <- y + a
        while (y[length(y)] < stop) y <- c(y, (y[length(y)] + x))
        y <- y[-1]
        y <- y[-which(y >= stop)]
        for (i in seq(along = y)){
                if (y[i] == 90) yy <- 0
                if (y[i] < 90) yy <- y[i] - 90
                if (y[i] > 90) yy <- abs(y[i] - 90)
                y[i] <- yy
        }
        y
  }
