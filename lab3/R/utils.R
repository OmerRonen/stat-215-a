library(here)
library(Rcpp)
library(logger)

source(here('R/similarity.R'))
sourceCpp(here('R/similarity.cpp'))

getX <- function(){
  # loading and extracting the relevant columns
  load(here("data/lingBinary.Rdata"))
  N = nrow(lingBinary)
  rownames(lingBinary) = c(1:N)
  X <- lingBinary[, c(7:ncol(lingBinary))]
  return(X)
}

subSampleCluster <- function(X, s, k){
  # sub-sample using a list of indices
  # returns a vector of clusters
  sub.X <- X[s,]
  l<- data.frame(kmeans(sub.X, k)$cluster)
  return(l)
}
.getSimilarityTime <-function(sim.func, l){
  # returns the running time of a similarity function
  start <- proc.time()
  sim.func(l,l)
  end <- proc.time()
  diff =  end-start
  return(diff[[3]])
  
}
getTimesDf <- function(){
  X <- getX()
  N <- nrow(X)
  out.len=5
  m.vec <- seq(0.1, 0.5, length.out = out.len)
  time.r <- c()
  time.cpp <- c()  
  time.cpp.slow <- c()
  # iterating over value of m, measuring time 
  # for each implementation
  for (m in m.vec){
  s <- sample(c(1:N), round(m*N))
  l <- unlist(subSampleCluster(X, s, 4))

  diff.cpp =  .getSimilarityTime(similarityRcpp, l)
  time.cpp <- c(time.cpp,diff.cpp)
  # log_info(paste('Cpp ', diff.cpp))
  
  diff.cpp.slow =  .getSimilarityTime(similarityRcppSlow, l)
  time.cpp.slow <- c(time.cpp.slow,diff.cpp.slow)
  # log_info(paste('Cpp slow ', diff.cpp.slow))
  
  diff.r =  .getSimilarityTime(similarity, l)
  time.r <- c(time.r, diff.r)
  # log_info(paste('R ', diff.r))
  
  }
  
  time.df <- data.frame(rep(m.vec, 3),
                        c(time.r, time.cpp, time.cpp.slow), 
                        c(rep('R', out.len),
                          rep('Cpp O(k1k2n)', out.len),
                          rep('Cpp O(n^2)', out.len)))
  colnames(time.df) <- c('m', 'Time', 'Implementation')
  return(time.df)
}