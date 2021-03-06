orderdClu <- function (res) {
  means <- fun.by.blocks(res, ignore.diag = TRUE)
  crit <- colSums(means) + rowSums(means)
  r <- order(crit, decreasing = TRUE)
  a <- IM(res)
  a <- a[as.vector(r), as.vector(r)]
  IMg <- array(NA, dim = c(1, max(r), max(r)))
  (IMg[1, , ] <- a)
  r <- rank(-crit, ties.method = "first")
  names(r) <- 1:max(clu(res))
  tclu <- r[as.character(clu(res))]
  return(list(tclu, IMg))
}

