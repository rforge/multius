orderdIM <- function (net, res) {
  means <- fun.by.blocks(x = net, clu =  res$clu, ignore.diag = TRUE)
  crit <- colSums(means) + rowSums(means)
  r <- order(crit, decreasing = TRUE)
  a <- res$IM[,,]
  a <- a[as.vector(r), as.vector(r)]
  return(a)
}
