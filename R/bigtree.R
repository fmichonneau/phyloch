bigtree <- function(phy, lo = 10){
  nt <- Ntip(phy)
  pdf("bigtree.pdf", height = nt/7)
  plot(phy, label.offset = lo, cex = .75)
  nodelabels(cex = .75, adj = c(1.1, -.3), frame = "n", col = "red")
  tiplabels(cex = .75, adj = c(-.25, .5), frame = "n", col = "red")
  dev.off()
  system("open bigtree.pdf")  
  unlink("bigtree.pdf")
}