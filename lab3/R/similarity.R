.constructC <- function(l){
  # constructing the C matrix
  # we check for the clusters vector l
  # where ll^T_ij is equal to l_i^2
  l <- as.matrix(l)
  n = length(l)
  square.mat <- matrix(rep(l^2, n), ncol=n)
  c <- (l %*% t(l) == square.mat)*1
  return(c)
}
.innerProd <- function(c1, c2){
  # performs inner product
  return(sum(c1*c2))
}
similarity <- function(l_1, l_2){
  # calculates the corr similarity score
  c_1 <- .constructC(l_1)
  c_2 <- .constructC(l_2)
  sim <- .innerProd(c_1, c_2)/sqrt(.innerProd(c_1, c_1)
                                   * .innerProd(c_2, c_2))
  return(sim)
}
