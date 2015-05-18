node.trans <- function(source, target, index = FALSE){
  
  nt <- Ntip(source); nn <- Nnode(source)
  int.source <- nt + 1:nn
  clades <- prop.part(source)
  n2s <- function(x, phy) phy$tip.label[x]
  clades <- lapply(X = clades, FUN = n2s, phy = source)
  int.target <- noi(phy = target, group = clades)
  if (any(is.na(int.target))) {
    id <- which(is.na(int.target))
    dd <- function(x, phy) {
      tips <- phy$tip.label
      xx <- which(!tips %in% x)
      x <- noi(phy, tips[xx])
      x <- c(xx, sister(phy, x))
      noi(phy, tips[x])
    }
    int.target[id] <- sapply(clades[id], dd, phy = target)
  }
  out <- cbind(source = int.source, target = int.target)
  if (index)
    out <- match(int.source, int.target)
  out
}


