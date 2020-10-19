library(here)

source(here('R/similarity.R'))
sourceCpp(here('R/similarity.cpp'))

getX <- function(){
  load(here("data/lingBinary.Rdata"))
  N = nrow(lingBinary)
  rownames(lingBinary) = c(1:N)
  X <- lingBinary[, c(7:ncol(lingBinary))]
  return(X)
}

subSampleCluster <- function(X, s, k){
  sub.X <- X[s,]
  l<- data.frame(kmeans(sub.X, k)$cluster)
  return(l)
}

getTimesDf <- function(){
  X <- getX()
  N <- nrow(X)
  out.len <- 5
  m.vec <- seq(0.1, 0.5, length.out = out.len)
  time.r <- c()
  time.cpp <- c()
  for (m in m.vec){
  s <- sample(c(1:N), round(m*N))
  l <- unlist(subSampleCluster(X, s, 4))
  
  start_r <- proc.time()
  similarity(l,l)
  end_r <- proc.time()
  diff =  end_r-start_r
  time.r <- c(time.r, diff[3])
  
  start_cpp <- proc.time()
  similarityRcpp(l,l)
  end_cpp <- proc.time()
  diff =  end_cpp-start_cpp
  time.cpp <- c(time.cpp,diff[3])
  
  }
  time.df <- data.frame(rep(m.vec, 2),
                        c(time.r, time.cpp), 
                        c(rep('R', out.len),
                          rep('Cpp', out.len)))
  colnames(time.df) <- c('m', 'Time', 'Implementation')
  return(time.df)
}