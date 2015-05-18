pal <- function(col, border = "light gray", ...) {

    n <- length(col) 
    plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
    axes = FALSE, xlab = "", ylab = "", ...) 
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = col)
}

# pal(sequential_hcl(100, h = 260, l = c(30, 90), power = 2.2))